{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Equiv (equivCmd) where

import Command.Utils
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.GraphViz (GraphvizCommand (Dot), runGraphvizCommand)
import Data.GraphViz.Types.Graph (toCanonical)
import Data.HashMap.Strict qualified as HM
import Data.LTS
import Data.Text qualified as T
import Data.WithData
import Diagnostic (toDiagnostic)
import Logging
import LqCCS.Parser.File (InputFileResult (..), scopedTo)
import Options (EquivOptions (..), EquivSource (..), Flag (..), Format (..), ProcOutput (..))
import Prettyprinter qualified as PP
import Prettyprinter.Util qualified as PP

import Api.Encode (equivDocument)
import LqCCS.Bisimulation qualified as Bisim
import LqCCS.EvalTagged qualified as EV
import LqCCS.Pretty (gv, gvMinimal, prettyLTS)
import LqCCS.QDist
import LqCCS.Quantum (minimalMeasures, minimalOps)
import LqCCS.Types (DeclMapFC, ProcessFC, Silence, Tag)

scopedTagEquiv :: String -> String -> InputFileResult -> [(Tag, Tag)]
scopedTagEquiv c1 c2 inp = concat (scopedTo c1 c2 inp.inpScopedTagEquiv)

scopedSilence :: String -> String -> InputFileResult -> Silence
scopedSilence c1 c2 inp = mconcat (scopedTo c1 c2 inp.inpScopedSilence)

runEquiv ::
    (MonadLog m) =>
    EquivOptions ->
    [(Tag, Tag)] ->
    Silence ->
    InputFileResult ->
    InputFileResult ->
    m Bool
runEquiv opts tagEquiv silence inp1 inp2 =
    withTypechecking inp1 $ \rho1 proc1 decls1 ->
        withTypechecking inp2 $ \rho2 proc2 decls2 -> do
            result <- timedEvalPar [(decls1, single rho1 proc1), (decls2, single rho2 proc2)]
            renderEquiv opts inp1.inpRaw tagEquiv silence result

runEquivNamed ::
    (MonadLog m) =>
    EquivOptions ->
    String ->
    String ->
    InputFileResult ->
    m Bool
runEquivNamed opts c1 c2 inp =
    withTypecheckingNamed c1 inp $ \rho1 proc1 decls1 ->
        withTypecheckingNamed c2 inp $ \rho2 proc2 decls2 -> do
            result <- timedEvalPar [(decls1, single rho1 proc1), (decls2, single rho2 proc2)]
            let te = equivTagEquiv opts ++ inp.inpTagEquiv ++ scopedTagEquiv c1 c2 inp
                sl = equivSilence opts <> inp.inpSilence <> scopedSilence c1 c2 inp
            renderEquiv opts inp.inpRaw te sl result

timedEvalPar ::
    (MonadLog m) =>
    [(DeclMapFC, QDist ProcessFC)] ->
    m (Either EV.EvalError EV.DecSemanticsFC)
timedEvalPar inputs = withPhase "evalPar" $ do
    let r = EV.evalPar inputs
    case r of
        Right lts -> HM.size lts.val.ltsStates `seq` pure ()
        Left _ -> pure ()
    pure r

renderEquiv ::
    (MonadLog m) =>
    EquivOptions ->
    String ->
    [(Tag, Tag)] ->
    Silence ->
    Either EV.EvalError EV.DecSemanticsFC ->
    m Bool
renderEquiv opts raw tagEquiv silence = \case
    Left errs -> do
        logWarn "evaluation produced an error"
        False <$ errDiagnostic (toDiagnostic raw errs)
    Right lts
        | equivFormat opts == Json -> do
            logDebug "encoding composed LTS to quarc.lts/1 JSON"
            warnIfNoLayout (HM.size lts.val.ltsStates)
            doc <- withPhase "encode" $ liftIO $ equivDocument (flagsToGVOptions (equivFlags opts)) equiv rooting tv (bisimTypeName equiv rooting (equivTagMode opts) tagEquiv) lts
            either bisimFailure ((True <$) . writeDocument (equivOutput opts)) doc
        | equivOutput opts == StdOut -> do
            logError "--stdout is only supported with --format json for equiv"
            False <$ liftIO (putStrLn "--stdout is only supported with --format json for equiv")
        | not (getMeta @EV.NoFreeInputsKey lts) -> do
            logInfo "processes have free inputs; not checkable"
            True <$ liftIO (putStrLn "The processes are not checkable")
        | otherwise -> do
            logInfo ("LTS: " <> T.pack (show (HM.size lts.val.ltsStates)) <> " states, " <> T.pack (show (HM.size lts.val.ltsTransitions)) <> " transitions")
            when (PrintLts `elem` equivFlags opts) $
                liftIO $
                    PP.putDocW 80 $
                        prettyLTS lts.val <> PP.line
            let dotGraph = toCanonical $ gv (flagsToGVOptions (equivFlags opts)) lts.val
            logDebug ("rendering composed graph to " <> T.pack outPath)
            void $ withPhase "graphviz" $ liftIO $ runGraphvizCommand Dot dotGraph (toGVOutput (equivFormat opts)) outPath
            liftIO $ putStrLn $ "transition system generated in " ++ outPath
            minOk <- case Bisim.minimalLTSUnder equiv tv lts of
                Left e -> bisimFailure e
                Right mlts -> fmap (const True) $ do
                    let mindotGraph = toCanonical $ gvMinimal (flagsToGVOptions (equivFlags opts)) mlts
                    logDebug ("rendering minimized graph to " <> T.pack minOutPath)
                    void $ withPhase "graphviz-min" $ liftIO $ runGraphvizCommand Dot mindotGraph (toGVOutput (equivFormat opts)) minOutPath
                    liftIO $ putStrLn $ "minimized transition system generated in " ++ minOutPath
            checkOk <- case lts.val.ltsInitial of
                [init1, init2] -> do
                    verdict <- withPhase "bisimulation" $ do
                        let b = Bisim.checkBisimilar equiv rooting tv lts init1 init2
                        either (const ()) (`seq` ()) b `seq` pure b
                    case verdict of
                        Left e -> bisimFailure e
                        Right bisim -> fmap (const True) $ do
                            logInfo (T.pack checkLabel <> " bisimilar: " <> T.pack (show bisim))
                            if bisim
                                then liftIO $ putStrLn ("the processes are " ++ checkLabel ++ " bisimilar!")
                                else liftIO $ putStrLn ("the processes are NOT " ++ checkLabel ++ " bisimilar!")
                _ -> do
                    logError "unexpected number of initial states"
                    False <$ liftIO (putStrLn "unexpected number of initial states")
            pure (minOk && checkOk)
  where
    equiv = equivEquivalence opts
    rooting = equivRooting opts
    tv = Bisim.tagViewFor (equivTagMode opts) tagEquiv silence
    checkLabel = Bisim.checkName equiv rooting
    stem = case equivOutput opts of
        OutputFile path -> path
        StdOut -> "equiv"
    outPath = stem ++ toExt (equivFormat opts)
    minOutPath = stem ++ "_minimal" ++ toExt (equivFormat opts)

bisimTypeName :: Bisim.Equivalence -> Bisim.Rooting -> Bisim.TagMode -> [(Tag, Tag)] -> String
bisimTypeName e r mode tags = base ++ suffix
  where
    base = case (e, mode, tags) of
        (Bisim.Strong, Bisim.FileTags, _ : _) -> ""
        (Bisim.Strong, _, _) -> "strong"
        _ -> map (\c -> if c == ' ' then '-' else c) (Bisim.checkName e r)
    suffix = case (mode, e, tags) of
        (Bisim.AngelicTags, _, _) -> "-angelic"
        (Bisim.FileTags, Bisim.Strong, _ : _) -> "tag"
        (Bisim.FileTags, _, _ : _) -> "-tag"
        _ -> ""

bisimFailure :: (MonadLog m) => Bisim.BisimError -> m Bool
bisimFailure e = do
    logError (T.pack (Bisim.bisimErrorMessage e))
    False <$ liftIO (putStrLn (Bisim.bisimErrorMessage e))

equivCmd :: (MonadLog m) => EquivOptions -> m Bool
equivCmd opts = case equivSource opts of
    TwoFiles f1 f2 ->
        withInputFile opts.equivTagging f1 minimalOps minimalMeasures $ \inp1 ->
            withInputFile opts.equivTagging f2 minimalOps minimalMeasures $ \inp2 ->
                runEquiv opts (opts.equivTagEquiv ++ inp1.inpTagEquiv ++ inp2.inpTagEquiv) (opts.equivSilence <> inp1.inpSilence <> inp2.inpSilence) inp1 inp2
    NamedConfigs f c1 c2 ->
        withInputFile opts.equivTagging f minimalOps minimalMeasures $ \inp ->
            runEquivNamed opts c1 c2 inp
