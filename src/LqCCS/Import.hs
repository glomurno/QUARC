{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : LqCCS.Import
Description : IO-level import resolution for .quarc files.

Resolves @import \"path\"@ directives by loading the referenced files,
merging their definitions (superoperators, measurements, proc declarations) into
the importing file's scope, and recursively handling transitive imports.

Cycle detection is performed using canonical file paths.
Each file is loaded independently and its resolved definitions are merged.
Duplicate proc names in the merged declaration list are silently accepted;
the type-checker resolves them using the last definition.
-}
module LqCCS.Import (
    loadQuarcFile,
    loadQuarcString,
    loadQuarcStringAs,
    scanImportPaths,
) where

import Control.Exception (IOException, evaluate, tryJust)
import Control.Monad (guard, when)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT, throwError)
import Data.Char (isSpace)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, utf8, withFile)

import LqCCS.Parser.File (InputFileResult (..), parseInput, parseInputFile)
import LqCCS.Types (Measure, Operator)

{- | Load a @.quarc@ file, resolving its @import@ directives relative to the
file's own directory.
-}
loadQuarcFile ::
    FilePath ->
    HashMap String Operator ->
    HashMap String Measure ->
    IO (Either String InputFileResult)
loadQuarcFile path ops meas = do
    absPath <- canonicalizePath path
    contents <- readFileUtf8 absPath
    runExceptT $ loadContents (Just absPath) (Set.singleton absPath) (takeDirectory absPath) ops meas contents

readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    _ <- evaluate (length s)
    pure s

{- | Parse @.quarc@ content with import directives resolved relative to
@baseDir@. Pass 'System.Directory.getCurrentDirectory' as @baseDir@ when
the input comes from stdin.
-}
loadQuarcString ::
    -- | Base directory for resolving relative imports.
    FilePath ->
    String ->
    HashMap String Operator ->
    HashMap String Measure ->
    IO (Either String InputFileResult)
loadQuarcString baseDir contents ops meas =
    runExceptT $ loadContents Nothing Set.empty baseDir ops meas contents

{- | Like 'loadQuarcString' but tags the content with a (possibly unsaved) @path@,
so source spans point at that path instead of the @\<input\>@ placeholder.
Relative imports resolve against the path's directory. Can be used to
live-check editor buffers.
-}
loadQuarcStringAs ::
    -- | Virtual path of the buffer (used for source spans and relative imports).
    FilePath ->
    String ->
    HashMap String Operator ->
    HashMap String Measure ->
    IO (Either String InputFileResult)
loadQuarcStringAs path contents ops meas =
    runExceptT $ loadContents (Just path) (Set.singleton path) (takeDirectory path) ops meas contents

scanImportPaths :: String -> [FilePath]
scanImportPaths = mapMaybe parseLine . lines . stripBlockComments
  where
    parseLine :: String -> Maybe FilePath
    parseLine line = do
        let stripped = dropWhile isSpace line
        guard $ not $ "--" `isPrefixOf` stripped
        rest <- stripPrefix "import " stripped
        case dropWhile isSpace rest of
            '"' : more -> case span (/= '"') more of
                (path, '"' : _) | not (null path) -> pure path
                _ -> mempty
            _ -> mempty

stripBlockComments :: String -> String
stripBlockComments = go
  where
    go [] = []
    go s@(c : cs)
        | "{-" `isPrefixOf` s = dropComment (drop 2 s)
        | otherwise = c : go cs
    dropComment [] = []
    dropComment s@(c : cs)
        | "-}" `isPrefixOf` s = go (drop 2 s)
        | c == '\n' = '\n' : dropComment cs
        | otherwise = dropComment cs

loadContents ::
    Maybe FilePath ->
    {- | Canonical paths of all ancestors in the current import chain.
    Used to detect import cycles.
    -}
    Set FilePath ->
    -- | Base directory for resolving relative imports in this file.
    FilePath ->
    HashMap String Operator ->
    HashMap String Measure ->
    String ->
    ExceptT String IO InputFileResult
loadContents path ancestors baseDir ops meas contents = do
    let relPaths = scanImportPaths contents
    imported <- traverse load relPaths
    let importedOps = HM.unions (inpCustomOps <$> imported)
    let importedMeas = HM.unions (inpCustomMeas <$> imported)
    let importedDecls = concatMap inpDecls imported
    let importedStates = HM.unions (inpNamedStates <$> imported)
    let mergedOps = importedOps <> ops
    let mergedMeas = importedMeas <> meas
    let parser = maybe (parseInput mergedOps mergedMeas importedStates) (parseInputFile mergedOps mergedMeas importedStates) path
    result <- liftEither $ parser contents
    pure $
        result
            { inpDecls = importedDecls ++ result.inpDecls
            , inpImports = []
            }
  where
    load rel = do
        absCandidate <- ExceptT $ tryJust @IOException (const $ Just $ "Import not found: " ++ rel) (canonicalizePath (baseDir </> rel))
        when (Set.member absCandidate ancestors) $
            throwError ("Import cycle detected: " ++ rel)
        importContents <- ExceptT $ tryJust @IOException (\e -> Just $ "Cannot read import '" ++ rel ++ "': " ++ show e) (readFileUtf8 absCandidate)
        loadContents
            (Just absCandidate)
            (Set.insert absCandidate ancestors)
            (takeDirectory absCandidate)
            ops
            meas
            importContents
