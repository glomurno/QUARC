{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Check (checkCmd) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text qualified as T
import Error.Diagnose (Diagnostic, Report (Err), addFile, addReport)
import Prettyprinter qualified as PP
import Prettyprinter.Util qualified as PP
import System.IO (hFlush, stdout)

import Api.Check
import Api.Contract (CheckKind (..), JsonDiagnostic (..), encodeCheckReport)
import Api.Diagnostics (tcDiagnostics)
import Command.Utils
import Diagnostic (toDiagnostic)
import Logging
import LqCCS.Parser.File (InputFileResult (..), lookupStartBlock)
import LqCCS.Quantum (minimalMeasures, minimalOps)
import LqCCS.Tagging (defaultTagPolicy)
import Options (CheckOptions (..), CheckTarget (..), ProcInput (..))

cliError :: (MonadLog m) => String -> String -> m ()
cliError raw msg = do
    let rep = Err Nothing msg [] []
    errDiagnostic (addFile (addReport mempty rep) "<cli>" raw :: Diagnostic String)

-- | Print one result: a status line on stdout, any diagnostic on stderr.
reportText :: (MonadLog m) => String -> Checked ann -> m ()
reportText raw (Checked kind name _ outcome) = do
    let label = targetLabel kind name
    case outcome of
        Passed doc -> do
            logInfo (T.pack label <> " typechecks")
            liftIO $ PP.putDocW 80 (PP.nest 4 (PP.pretty label <> PP.line <> doc) <> PP.line)
        Failed err -> do
            logWarn (T.pack label <> ": typechecking failed")
            liftIO $ putStrLn label >> hFlush stdout
            errDiagnostic (toDiagnostic raw err)
        TooSmall expected given -> do
            logWarn (T.pack label <> ": quantum state too small")
            liftIO $ putStrLn label >> hFlush stdout
            cliError raw (tooSmallMsg expected given)
        Skipped deps -> do
            logWarn (T.pack label <> ": skipped")
            liftIO $ putStrLn (label ++ " (" ++ skippedMsg deps ++ ")")

summary :: [Checked ann] -> String
summary rs =
    count "process" "processes" CheckProcess
        ++ ", "
        ++ count "start block" "start blocks" CheckStartBlock
        ++ " checked: "
        ++ show (length [() | Checked _ _ _ o <- rs, isFailure o])
        ++ " failed, "
        ++ show (length [() | Checked _ _ _ Skipped{} <- rs])
        ++ " skipped"
  where
    count one other kind =
        let n = length [() | Checked k _ _ _ <- rs, sameKind k kind]
         in show n ++ " " ++ (if n == 1 then one else other)
    sameKind CheckProcess CheckProcess = True
    sameKind CheckStartBlock CheckStartBlock = True
    sameKind _ _ = False

-- | Print the results; returns whether every check passed.
emit :: (MonadLog m) => Bool -> InputFileResult -> [Checked ann] -> m Bool
emit json inp rs = do
    emit' json inp rs
    pure (allPassed rs)

emit' :: (MonadLog m) => Bool -> InputFileResult -> [Checked ann] -> m ()
emit' True _ rs = liftIO $ BLC.putStrLn (encodeCheckReport [] (toResult <$> rs))
emit' False inp rs = do
    mapM_ (reportText inp.inpRaw) rs
    unless (null rs) $ liftIO $ putStrLn ""
    liftIO $ putStrLn (summary rs)

{- | Report an error that prevents any per-target result (e.g. a missing start
block); always returns 'False'.
-}
globalError :: (MonadLog m) => Bool -> InputFileResult -> [JsonDiagnostic] -> m () -> m Bool
globalError True _ ds _ = False <$ liftIO (BLC.putStrLn (encodeCheckReport ds []))
globalError False _ _ text = False <$ text

-- | Check the input; returns whether every check passed.
check :: (MonadLog m) => Bool -> CheckTarget -> InputFileResult -> m Bool
check json target inp = case target of
    CheckStart name -> case lookupStartBlock name inp of
        Nothing -> do
            let msg = "No start block named '" ++ name ++ "' found in input"
            logWarn "missing start block"
            globalError json inp [JsonDiagnostic "error" msg Nothing] (cliError inp.inpRaw msg)
        Just b -> withPhase "typecheck" (pure $! checkStart inp b) >>= emit json inp . pure
    _ -> do
        logInfo "typechecking processes and start blocks"
        result <- withPhase "typecheck" $ pure $! checkAll (target == CheckAll) inp
        case result of
            Left err -> do
                logWarn "typechecking failed"
                globalError json inp (tcDiagnostics err) (errDiagnostic (toDiagnostic inp.inpRaw err))
            Right rs -> do
                unless (json || not (null rs)) $ logWarn "nothing to check: no processes or start blocks found"
                emit json inp rs

{- | Run @check@; returns whether every check passed. Failing to load or parse
the input counts as a failure.
-}
checkCmd :: (MonadLog m) => CheckOptions -> m Bool
checkCmd opts = case opts.checkInput of
    StdIn -> withInputStdin defaultTagPolicy minimalOps minimalMeasures k
    InputFile path -> withInputFile defaultTagPolicy path minimalOps minimalMeasures k
  where
    k = check opts.checkJson opts.checkTarget
