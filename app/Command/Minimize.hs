{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Minimize (minimizeCmd) where

import Control.Monad (void, when, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.GraphViz (GraphvizCommand (Dot), graphvizWithHandle, runGraphvizCommand)
import Data.GraphViz.Types.Graph (toCanonical)
import Data.HashMap.Strict qualified as HM
import Data.IntSet qualified as IS
import Data.LTS (LTS (..))
import Data.List (intercalate)
import Data.Text qualified as T
import Data.WithData
import Prettyprinter qualified as PP
import Prettyprinter.Util qualified as PP
import System.IO (hGetContents)

import Api.Encode (minimizeDocument)
import Command.Utils
import Diagnostic (toDiagnostic)
import Logging
import LqCCS.Bisimulation qualified as Bisim
import LqCCS.EvalTagged qualified as EV
import LqCCS.Parser.File (InputFileResult (..))
import LqCCS.Pretty (gvMinimal, prettyLTS)
import LqCCS.QDist
import LqCCS.Quantum (minimalMeasures, minimalOps)
import LqCCS.Types (DeclMap, Process)
import Options (Flag (..), Format (..), MinimizeOptions (..), ProcInput (..), ProcOutput (..))

timedEval :: (MonadLog m) => QState -> Process meta -> DeclMap meta -> m (Either EV.EvalError (EV.DecSemantics meta))
timedEval rho proc decls = withPhase "eval" $ do
    let r = EV.eval decls (single rho proc)
    -- Force the LTS spine inside the timed phase so the measurement reflects
    -- the actual graph construction rather than an unforced thunk.
    case r of
        Right lts -> HM.size lts.val.ltsStates `seq` pure ()
        Left _ -> pure ()
    pure r

minimize :: (MonadLog m) => MinimizeOptions -> InputFileResult -> m Bool
minimize opts inp = withEntry inp $ \rho proc decls -> do
    result <- timedEval rho proc decls
    case result of
        Left errs -> do
            logWarn "evaluation produced an error"
            False <$ errDiagnostic (toDiagnostic (inpRaw inp) errs)
        Right lts -> do
            logInfo ("LTS: " <> T.pack (show (HM.size lts.val.ltsStates)) <> " states, " <> T.pack (show (HM.size lts.val.ltsTransitions)) <> " transitions")
            when (PrintLts `elem` minimizeFlags opts) $
                liftIO $
                    PP.putDocW 80 $
                        prettyLTS lts.val <> PP.line
            let tv = Bisim.tagViewFor (minimizeTagMode opts) (minimizeTagEquiv opts ++ inp.inpTagEquiv) (minimizeSilence opts <> inp.inpSilence)
                equiv = minimizeEquivalence opts
            case minimizeFormat opts of
                Json -> do
                    logDebug "encoding minimized LTS to quarc.lts/1 JSON"
                    doc <- withPhase "encode" $ liftIO $ minimizeDocument (flagsToGVOptions (minimizeFlags opts)) equiv tv lts
                    either bisimFailure ((True <$) . writeDocument (minimizeOutput opts)) doc
                _ -> do
                    result' <- withPhase "bisimulation" $ do
                        let r = (,) <$> Bisim.bisimilarityClassesUnder equiv tv lts <*> Bisim.minimalLTSUnder equiv tv lts
                        either (const ()) (\(cs, _) -> length cs `seq` ()) r `seq` pure r
                    case result' of
                        Left e -> bisimFailure e
                        Right (classes, mlts) -> fmap (const True) $ do
                            logInfo (T.pack (Bisim.equivalenceName equiv) <> " bisimilarity: " <> T.pack (show (length classes)) <> " equivalence classes")
                            liftIO $ putStrLn "Bisimilarity classes:"
                            for_ classes $ \cl -> do
                                liftIO $ putStr "{"
                                liftIO $ putStr $ intercalate ", " [show i | i <- IS.toList cl]
                                liftIO $ putStrLn "}"
                            let dotGraph = toCanonical $ gvMinimal (flagsToGVOptions (minimizeFlags opts)) mlts
                            case minimizeOutput opts of
                                StdOut -> do
                                    logDebug "rendering minimized graph to stdout"
                                    liftIO $ graphvizWithHandle Dot dotGraph (toGVOutput (minimizeFormat opts)) (putStr <=< hGetContents)
                                OutputFile path -> do
                                    let outPath = path ++ toExt (minimizeFormat opts)
                                    logDebug ("rendering minimized graph to " <> T.pack outPath)
                                    void $ withPhase "graphviz" $ liftIO $ runGraphvizCommand Dot dotGraph (toGVOutput (minimizeFormat opts)) outPath
                                    liftIO $ putStrLn $ "minimized transition system generated in " ++ outPath
  where
    withEntry = maybe withTypechecking withTypecheckingNamed (minimizeConfig opts)
    bisimFailure e = do
        logError (T.pack (Bisim.bisimErrorMessage e))
        False <$ liftIO (putStrLn (Bisim.bisimErrorMessage e))

minimizeCmd :: (MonadLog m) => MinimizeOptions -> m Bool
minimizeCmd opts = case minimizeInput opts of
    StdIn -> withInputStdin opts.minimizeTagging minimalOps minimalMeasures (minimize opts)
    InputFile path -> withInputFile opts.minimizeTagging path minimalOps minimalMeasures (minimize opts)
