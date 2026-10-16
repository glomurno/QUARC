{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Command.Utils where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.GraphViz (GraphvizOutput)
import Data.GraphViz qualified as GV
import Data.HashMap.Strict (HashMap)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Void (Void)
import Error.Diagnose (Diagnostic, Report (Err), TabSize (TabSize), WithUnicode (..), addFile, addReport, defaultStyle, printDiagnostic, stderr)
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Prettyprinter (Pretty)
import System.Directory (getCurrentDirectory)

import Api.Contract (LtsDocument, encodeDocument)
import Api.Layout (layout_max_nodes)
import Diagnostic (toDiagnostic)
import Logging
import LqCCS.Import (loadQuarcFile, loadQuarcString)
import LqCCS.Parser.File
import LqCCS.Pretty (GVGenOptions (..), PrettyOptions (..), defaultPrettyOptions)
import LqCCS.QDist (QState (..))
import LqCCS.Tagging (TagPolicy, applyTagPolicy)
import LqCCS.Typecheck qualified as TC
import LqCCS.Types (DeclMapFC, Measure, Operator, OpsMap (..), ProcessFC, minSize)
import Options (Flag (..), Format (..), ProcOutput (..))

instance HasHints Void msg where
    hints _ = mempty

errDiagnostic :: (MonadIO m, Pretty msg) => Diagnostic msg -> m ()
errDiagnostic = printDiagnostic stderr WithUnicode (TabSize 2) defaultStyle

withInputStdin :: (MonadLog m) => TagPolicy -> HashMap String Operator -> HashMap String Measure -> (InputFileResult -> m Bool) -> m Bool
withInputStdin policy ops meas k = do
    logInfo "reading process from stdin"
    contents <- liftIO getContents
    cwd <- liftIO getCurrentDirectory
    result <- withPhase "parse" $ liftIO $ loadQuarcString cwd contents ops meas
    handleLoadResult policy result k

withInputFile :: (MonadLog m) => TagPolicy -> FilePath -> HashMap String Operator -> HashMap String Measure -> (InputFileResult -> m Bool) -> m Bool
withInputFile policy input ops meas k = do
    logInfo ("loading file " <> T.pack input)
    result <- withPhase "parse" $ liftIO $ try @IOException (loadQuarcFile input ops meas)
    case result of
        Left ioErr -> do
            logError ("could not read " <> T.pack input <> ": " <> T.pack (show ioErr))
            let rep = Err Nothing (show ioErr) [] []
            False <$ errDiagnostic (addReport mempty rep :: Diagnostic String)
        Right loadResult -> handleLoadResult policy loadResult k

handleLoadResult :: (MonadLog m) => TagPolicy -> Either String InputFileResult -> (InputFileResult -> m Bool) -> m Bool
handleLoadResult _ (Left err) _ = do
    logError ("load failed: " <> T.pack err)
    let rep = Err Nothing err [] []
    False <$ errDiagnostic (addReport mempty rep :: Diagnostic String)
handleLoadResult policy (Right inpFile) k = do
    logDebug "input loaded and parsed"
    k (applyTagPolicy policy inpFile)

withTypechecking :: (MonadLog m) => InputFileResult -> (QState -> ProcessFC -> DeclMapFC -> m Bool) -> m Bool
withTypechecking inp k = case inpDefaultEntry inp of
    Nothing -> do
        let rep = Err Nothing "No start block found in input" [] []
        let diag = addFile (addReport mempty rep) "<cli>" inp.inpRaw :: Diagnostic String
        False <$ errDiagnostic diag
    Just entry -> typecheckEntry "<default>" inp entry k

withTypecheckingNamed :: (MonadLog m) => String -> InputFileResult -> (QState -> ProcessFC -> DeclMapFC -> m Bool) -> m Bool
withTypecheckingNamed name inp k =
    case lookupStartBlock name inp >>= listToMaybe . sbEntries of
        Nothing -> do
            let rep = Err Nothing ("No start block named '" ++ name ++ "' found in input") [] []
            let diag = addFile (addReport mempty rep) "<cli>" inp.inpRaw :: Diagnostic String
            False <$ errDiagnostic diag
        Just entry -> typecheckEntry (T.pack name) inp entry k

typecheckEntry ::
    (MonadLog m) =>
    T.Text ->
    InputFileResult ->
    DistEntry ->
    (QState -> ProcessFC -> DeclMapFC -> m Bool) ->
    m Bool
typecheckEntry name inp entry k = do
    logInfo ("typechecking start block " <> name)
    result <- withPhase "typecheck" $ pure $! TC.inferWithDecls (OpsMap inp.inpCustomOps inp.inpCustomMeas) inp.inpDecls entry.deProc
    case result of
        Left errs -> do
            logWarn ("typechecking failed for " <> name)
            False <$ errDiagnostic (toDiagnostic inp.inpRaw errs)
        Right (proc, decls) ->
            if entry.deRho.dim < minSize proc
                then do
                    logError ("quantum state too small: expected >=" <> T.pack (show (minSize proc)) <> ", given " <> T.pack (show entry.deRho.dim))
                    let rep = Err Nothing ("quantum state smaller then required: expected >=" ++ show (minSize proc) ++ ", given " ++ show entry.deRho.dim) [] []
                    let diag = addFile (addReport mempty rep) "<cli>" inp.inpRaw :: Diagnostic String
                    False <$ errDiagnostic diag
                else do
                    logDebug ("typecheck ok (state dim " <> T.pack (show entry.deRho.dim) <> ")")
                    k entry.deRho proc decls

flagsToGVOptions :: [Flag] -> GVGenOptions
flagsToGVOptions = foldr applyFlag (GVGenOptions False False False defaultPrettyOptions)
  where
    applyFlag Simplify o = o{simplifyLTS = True}
    applyFlag NoNames o = o{noProcNames = True}
    applyFlag OnlyDots o = o{onlyDots = True}
    applyFlag Sugar o = o{gvPrettyOptions = PrettyOptions{sugar = True}}
    applyFlag PrintLts o = o

toGVOutput :: Format -> GraphvizOutput
toGVOutput Png = GV.Png
toGVOutput Svg = GV.Svg
toGVOutput Json = error "toGVOutput: json format is produced by the JSON encoder, not graphviz"

toExt :: Format -> String
toExt Png = ".png"
toExt Svg = ".svg"
toExt Json = ".json"

writeDocument :: (MonadIO m) => ProcOutput -> LtsDocument -> m ()
writeDocument StdOut doc = liftIO $ BLC.putStrLn (encodeDocument doc)
writeDocument (OutputFile path) doc = liftIO $ do
    let outPath = path ++ toExt Json
    BL.writeFile outPath (encodeDocument doc)
    putStrLn $ "JSON document written to " ++ outPath

warnIfNoLayout :: (MonadLog m) => Int -> m ()
warnIfNoLayout n =
    when (n > layout_max_nodes) $
        logWarn ("graph has " <> T.pack (show n) <> " nodes (> " <> T.pack (show layout_max_nodes) <> "); skipping graphviz layout, the document will have no positions")
