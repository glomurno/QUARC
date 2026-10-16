{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Eval (evalCmd) where

import Control.Monad (void, when, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.GraphViz (GraphvizCommand (Dot), graphvizWithHandle, runGraphvizCommand)
import Data.GraphViz.Types.Graph (toCanonical)
import Data.HashMap.Strict qualified as HM
import Data.LTS (LTS (..))
import Data.Text qualified as T
import Data.WithData
import Prettyprinter qualified as PP
import Prettyprinter.Util qualified as PP
import System.IO (hGetContents)

import Api.Encode (evalDocument)
import Command.Utils
import Diagnostic (toDiagnostic)
import Logging
import LqCCS.EvalTagged qualified as EV
import LqCCS.Parser.File (InputFileResult (..))
import LqCCS.Pretty (gv, prettyLTS)
import LqCCS.QDist
import LqCCS.Quantum (minimalMeasures, minimalOps)
import LqCCS.Types (DeclMap, Process)
import Options (EvalOptions (..), Flag (..), Format (..), ProcInput (..), ProcOutput (..))

timedEval :: (MonadLog m) => QState -> Process meta -> DeclMap meta -> m (Either EV.EvalError (EV.DecSemantics meta))
timedEval rho proc decls = withPhase "eval" $ do
    let r = EV.eval decls (single rho proc)
    -- Force the LTS spine inside the timed phase so the measurement reflects
    -- the actual graph construction rather than an unforced thunk.
    case r of
        Right lts -> HM.size lts.val.ltsStates `seq` pure ()
        Left _ -> pure ()
    pure r

runEval :: (MonadLog m) => EvalOptions -> InputFileResult -> m Bool
runEval opts inp = withEntry inp $ \rho proc decls ->
    timedEval rho proc decls >>= \case
        Left errs -> do
            logWarn "evaluation produced an error"
            False <$ errDiagnostic (toDiagnostic (inpRaw inp) errs)
        Right lts -> fmap (const True) $ do
            let nStates = HM.size lts.val.ltsStates
                nTrans = HM.size lts.val.ltsTransitions
            logInfo ("LTS: " <> T.pack (show nStates) <> " states, " <> T.pack (show nTrans) <> " transitions")
            case evalFormat opts of
                Json -> do
                    logDebug "encoding LTS to quarc.lts/1 JSON"
                    warnIfNoLayout nStates
                    doc <- withPhase "encode" $ liftIO $ evalDocument (flagsToGVOptions (evalFlags opts)) lts
                    writeDocument (evalOutput opts) doc
                _ -> do
                    when (PrintLts `elem` evalFlags opts) $
                        liftIO $
                            PP.putDocW 80 $
                                prettyLTS lts.val <> PP.line
                    let dotGraph = toCanonical $ gv (flagsToGVOptions (evalFlags opts)) lts.val
                    case evalOutput opts of
                        StdOut -> do
                            logDebug "rendering graph to stdout"
                            liftIO $ graphvizWithHandle Dot dotGraph (toGVOutput (evalFormat opts)) (putStr <=< hGetContents)
                        OutputFile path -> do
                            let outPath = path ++ toExt (evalFormat opts)
                            logDebug ("rendering graph to " <> T.pack outPath)
                            void $ withPhase "graphviz" $ liftIO $ runGraphvizCommand Dot dotGraph (toGVOutput (evalFormat opts)) outPath
                            liftIO $ putStrLn $ "transition system generated in " ++ outPath
  where
    withEntry = maybe withTypechecking withTypecheckingNamed (evalConfig opts)

evalCmd :: (MonadLog m) => EvalOptions -> m Bool
evalCmd opts = case evalInput opts of
    StdIn -> withInputStdin opts.evalTagging minimalOps minimalMeasures (runEval opts)
    InputFile path -> withInputFile opts.evalTagging path minimalOps minimalMeasures (runEval opts)
