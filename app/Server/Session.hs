{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Server.Session
Description : Transport-free request handler + session cache for the sidecar.

Holds evaluated LTSs in an in-memory cache keyed by an integer handle, so the
client can fetch a node's density matrices lazily (@getState@) without re-running
the evaluation. 'handle' is pure with respect to transport — 'Command.Serve' wraps
it in a stdio loop, and a future HTTP\/WS server would reuse it unchanged.
-}
module Server.Session (
    Report,
    Session,
    handle,
    handleWith,
    newSession,
    newSessionNoLayout,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.GraphViz (GraphvizCommand (Dot), GraphvizOutput (Png, Svg), graphvizWithHandle, runGraphvizCommand)
import Data.List (stripPrefix)
import Data.GraphViz.Types.Graph (DotGraph)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.LTS (LTS (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.WithData (WithData (..))
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStr, hSetEncoding, utf8, withFile)
import Text.Read (readMaybe)

import Api.Contract (JsonDiagnostic (..), SourceSpan (..))
import Api.Check (checkAll, checkedDiagnostics)
import Api.Diagnostics (evalDiagnostics, tcDiagnostics)
import Api.Encode (
    Layout,
    buildStateIndex,
    emptyLayout,
    equivDocumentSummaryWith,
    equivMinimizedDocumentWith,
    equivQuotient,
    evalDocumentSummaryWith,
    minimizeDocumentWith,
    nodeDetailAt,
 )
import Api.Layout (layoutDot)
import LqCCS.Bisimulation (Equivalence (..), Rooting (..), TagMode (..), TagView, bisimErrorMessage, minimalLTSUnder, tagView, tagViewFor)
import LqCCS.EvalTagged qualified as EV
import LqCCS.Import (loadQuarcFile, loadQuarcStringAs)
import LqCCS.Parser.File
import LqCCS.Pretty (GVGenOptions (..), PrettyOptions (..), defaultPrettyOptions, gv, gvMinimal)
import LqCCS.QDist (QDist, QState (..), single)
import LqCCS.Tagging (applyTagPolicy)
import LqCCS.Quantum (minimalMeasures, minimalOps)
import LqCCS.Typecheck qualified as TC
import LqCCS.Types (DensityOp, Measure, Operator, OpsMap (..), ProcessFC, Silence, Tag, minSize)
import Options (Flag (..))
import Server.Protocol

{- | Build graphviz\/pretty render options from request flags. Mirrors
@Command.Utils.flagsToGVOptions@ but kept here so the sidecar layer does not
depend on CLI plumbing.
-}
flagsToGVOptions :: [Flag] -> GVGenOptions
flagsToGVOptions = foldr apply (GVGenOptions False False False defaultPrettyOptions)
  where
    apply Simplify o = o{simplifyLTS = True}
    apply NoNames o = o{noProcNames = True}
    apply OnlyDots o = o{onlyDots = True}
    apply Sugar o = o{gvPrettyOptions = PrettyOptions{sugar = True}}
    apply PrintLts o = o

-- | A cached evaluation: render options + the per-node detail index for @getState@.
data Cached = Cached !GVGenOptions !(IntMap (QDist ProcessFC, DensityOp))

{- | Sidecar session state: a handle counter + cache, custom op\/measurement
tables, and whether to run graphviz layout (disabled in tests).
-}
data Session = Session
    { store :: !(MVar (Int, IntMap Cached))
    , sessOps :: !(HashMap String Operator)
    , sessMeas :: !(HashMap String Measure)
    , sessLayout :: !Bool
    }

-- | A fresh session that computes graphviz layout for the graphs it returns.
newSession :: IO Session
newSession = mkSession True

-- | A fresh session that skips graphviz layout (positions empty); for tests.
newSessionNoLayout :: IO Session
newSessionNoLayout = mkSession False

mkSession :: Bool -> IO Session
mkSession layout = do
    ref <- newMVar (0, IM.empty)
    pure (Session ref minimalOps minimalMeasures layout)

{- | Progress callback: the phase a request entered (@parse@, @evaluate@,
@minimize@, @compare@, @layout@, @render@) and, once known, its state count.
-}
type Report = String -> Maybe Int -> IO ()

-- | Handle one request, producing a response that echoes its id.
handle :: Session -> Request -> IO Response
handle = handleWith (\_ _ -> pure ())

-- | 'handle', reporting progress through the given callback.
handleWith :: Report -> Session -> Request -> IO Response
handleWith report sess (Request i meth) = Response i <$> dispatch report sess meth

dispatch :: Report -> Session -> ReqMethod -> IO (Either RpcError Result)
dispatch report sess = \case
    ReqEval src mconfig flags -> do
        ep <- prepare report sess src mconfig
        case ep of
            Left e -> pure (Left e)
            Right (_, rho, proc, decls) -> do
                report "evaluate" Nothing
                evalAndCache report sess (flagsToGVOptions flags) $
                    EvalGraph <$> evalEither (EV.eval decls (single rho proc))
    ReqMinimize src mconfig flags equiv msilence -> do
        ep <- prepare report sess src mconfig
        case ep of
            Left e -> pure (Left e)
            Right (inp, rho, proc, decls) -> do
                report "evaluate" Nothing
                evalAndCache report sess (flagsToGVOptions flags) $
                    MinGraph equiv (minimizeView inp msilence) <$> evalEither (EV.eval decls (single rho proc))
    ReqEquiv src c1 c2 flags mbt msilence minimized -> do
        report "parse" Nothing
        ep <- prepare2 sess src c1 c2
        case ep of
            Left e -> pure (Left e)
            Right (inp, (rho1, p1, d1), (rho2, p2, d2)) ->
                let bt = fromMaybe "strong" mbt
                 in case parseBisimType bt of
                        Nothing -> pure (Left (RpcError appErrorCode ("unknown bisimType: " ++ bt) []))
                        Just (equiv, rooting, mode) ->
                            let tags = inp.inpTagEquiv ++ scopedTagEquiv c1 c2 inp
                                silence = fromMaybe (inp.inpSilence <> mconcat (scopedTo c1 c2 inp.inpScopedSilence)) msilence
                                view = tagViewFor mode tags silence
                             in report "evaluate" Nothing
                                    >> evalAndCache report sess (flagsToGVOptions flags) (
                                    EquivGraph equiv rooting view bt minimized
                                        <$> evalEither (EV.evalPar [(d1, single rho1 p1), (d2, single rho2 p2)]))
    ReqGetState h sid -> do
        (_, m) <- readMVar sess.store
        pure $ case IM.lookup h m of
            Nothing -> Left (RpcError appErrorCode ("unknown handle: " ++ show h) [])
            Just (Cached opts idx) -> case nodeDetailAt opts idx sid of
                Nothing -> Left (RpcError appErrorCode ("unknown state id: " ++ show sid) [])
                Just d -> Right (RDetail d)
    ReqCheck src -> checkSource sess src
    ReqOutline src -> outlineSource sess src
    ReqExport src mconfig flags kind format equiv msilence mpath -> exportGraph report sess (flagsToGVOptions flags) src mconfig kind format equiv msilence mpath
    ReqSave path text -> saveSource path text
    ReqClose h -> do
        modifyMVar_ sess.store (\(n, m) -> pure (n, IM.delete h m))
        pure (Right ROk)
    -- Cancellation needs the transport's worker threads ('Command.Serve');
    -- reaching the transport-free handler means there is nothing to cancel.
    ReqCancel _ -> pure (Right ROk)

{- | The tag view for minimizing a single config: the file's tag equivalences,
and its global silence declarations unless the request overrides them.
-}
minimizeView :: InputFileResult -> Maybe Silence -> TagView
minimizeView inp msilence = tagView inp.inpTagEquiv (fromMaybe inp.inpSilence msilence)

{- | Decode an @equiv@ @bisimType@: @strong@, @branching@ or @rooted-branching@,
optionally suffixed with @-tag@ to apply the file's tag equivalences or
@-angelic@ to identify all tags (@tag@ alone means strong with the file's tag
equivalences); no suffix means no tag equivalence.
-}
parseBisimType :: String -> Maybe (Equivalence, Rooting, TagMode)
parseBisimType "tag" = Just (Strong, Unrooted, FileTags)
parseBisimType bt = case splitMode bt of
    (base, mode) -> (\(e, r) -> (e, r, mode)) <$> lookup base kinds
  where
    kinds = [("strong", (Strong, Unrooted)), ("branching", (Branching, Unrooted)), ("rooted-branching", (Branching, Rooted))]
    splitMode s
        | Just b <- stripSuffix "-tag" s = (b, FileTags)
        | Just b <- stripSuffix "-angelic" s = (b, AngelicTags)
        | otherwise = (s, DemonicTags)
    stripSuffix suf s = reverse <$> stripPrefix (reverse suf) (reverse s)

-- | Lift an evaluation result into the error channel with diagnostics.
evalEither :: Either EV.EvalError a -> Either RpcError a
evalEither = either (Left . RpcError appErrorCode "evaluation error" . evalDiagnostics) Right

-- | What a successful evaluation produced, before layout + encoding.
data Graph
    = EvalGraph EV.DecSemanticsFC
    | MinGraph Equivalence TagView EV.DecSemanticsFC
    | EquivGraph Equivalence Rooting TagView String Bool EV.DecSemanticsFC

{- | Lay out + encode an evaluated graph, caching its detail index under a fresh
handle, and reply with @{handle, document}@.
-}
evalAndCache :: Report -> Session -> GVGenOptions -> Either RpcError Graph -> IO (Either RpcError Result)
evalAndCache _ _ _ (Left e) = pure (Left e)
evalAndCache report sess opts (Right g) = do
    -- Matching the Either above ran the evaluation; the state count is cheap now.
    states <- evaluate (HM.size (graphLts g).val.ltsStates)
    let phase p = report p (Just states)
    case g of
        EvalGraph lts -> do
            phase "layout"
            layout <- doLayout sess (gv opts lts.val)
            finish lts (Right (evalDocumentSummaryWith opts layout lts))
        EquivGraph equiv rooting tv bt False lts -> do
            phase "layout"
            layout <- doLayout sess (gv opts lts.val)
            phase "compare"
            finish lts (equivDocumentSummaryWith opts equiv rooting tv bt layout lts)
        EquivGraph equiv rooting tv bt True lts -> do
            phase "compare"
            case equivQuotient equiv tv lts of
                Left e -> pure (Left (bisimRpcError e))
                Right (mlts, _) -> do
                    phase "layout"
                    layout <- doLayout sess (gvMinimal opts mlts)
                    finish lts (equivMinimizedDocumentWith opts equiv rooting tv bt layout lts)
        MinGraph equiv tv lts -> do
            phase "minimize"
            case minimalLTSUnder equiv tv lts of
                Left e -> pure (Left (bisimRpcError e))
                Right mlts -> do
                    phase "layout"
                    layout <- doLayout sess (gvMinimal opts mlts)
                    finish lts (minimizeDocumentWith opts equiv tv layout lts)
  where
    graphLts = \case
        EvalGraph lts -> lts
        MinGraph _ _ lts -> lts
        EquivGraph _ _ _ _ _ lts -> lts
    finish _ (Left e) = pure (Left (bisimRpcError e))
    finish lts (Right doc) = do
        h <- cacheStore sess opts (buildStateIndex lts.val)
        pure (Right (RDocument h doc))
    bisimRpcError e = RpcError appErrorCode (bisimErrorMessage e) []

-- | Load a source, select a start block, and typecheck it.
prepare :: Report -> Session -> Source -> Maybe String -> IO (Either RpcError (InputFileResult, QState, ProcessFC, TC.DeclMapFC))
prepare report sess src mconfig = do
    report "parse" Nothing
    er <- loadSource sess src
    pure $ do
        inp <- er
        entry <- selectEntry mconfig inp
        (rho, proc, decls) <- typecheckEntry inp entry
        pure (inp, rho, proc, decls)

-- | Load a source and select\/typecheck two named start blocks (for @equiv@).
prepare2 :: Session -> Source -> String -> String -> IO (Either RpcError (InputFileResult, (QState, ProcessFC, TC.DeclMapFC), (QState, ProcessFC, TC.DeclMapFC)))
prepare2 sess src c1 c2 = do
    er <- loadSource sess src
    pure $ do
        inp <- er
        e1 <- selectEntry (Just c1) inp
        e2 <- selectEntry (Just c2) inp
        t1 <- typecheckEntry inp e1
        t2 <- typecheckEntry inp e2
        pure (inp, t1, t2)

loadSource :: Session -> Source -> IO (Either RpcError InputFileResult)
loadSource sess src = do
    -- `loadQuarcFile` reads the file in IO before yielding its `Either`, so a bad
    -- path (missing file, permissions) throws rather than returning `Left`;
    -- catch it here so it surfaces as an RPC error instead of killing the sidecar.
    r <- try $ case src.sourceInput of
        SourceFile f -> loadQuarcFile f sess.sessOps sess.sessMeas
        SourceText t p -> loadQuarcStringAs p t sess.sessOps sess.sessMeas
    pure $ toErr $ case r of
        Left (e :: SomeException) -> Left (show e)
        Right res -> applyTagPolicy src.sourceTagging <$> res
  where
    toErr = either (\msg -> Left (RpcError appErrorCode msg [JsonDiagnostic "error" msg (parseErrorSpan msg)])) Right

{- | Best-effort span from a parse-error message. Megaparsec prints each error
starting with a @\<name\>:\<line\>:\<col\>:@ header, so recover the first
line\/col (1-based, matching 'Api.Diagnostics.fcSpan') and point the editor at
that position instead of falling back to line 1. Load failures without such a
header (e.g. import IO errors) yield 'Nothing'.
-}
parseErrorSpan :: String -> Maybe SourceSpan
parseErrorSpan msg = do
    hdr <- listToMaybe (lines msg)
    let core = if not (null hdr) && last hdr == ':' then init hdr else hdr
    case reverse (splitOnColon core) of
        (colS : lineS : _) -> do
            l <- readMaybe lineS
            c <- readMaybe colS
            pure (SourceSpan "" l c l c)
        _ -> Nothing
  where
    splitOnColon s = case break (== ':') s of
        (a, _ : rest) -> a : splitOnColon rest
        (a, []) -> [a]

selectEntry :: Maybe String -> InputFileResult -> Either RpcError DistEntry
selectEntry Nothing inp =
    maybe (Left (RpcError appErrorCode "No start block found in input" [])) Right (inpDefaultEntry inp)
selectEntry (Just name) inp =
    maybe (Left (RpcError appErrorCode ("No start block named '" ++ name ++ "' found in input") [])) Right $
        lookupStartBlock name inp >>= listToMaybe . (.sbEntries)

typecheckEntry :: InputFileResult -> DistEntry -> Either RpcError (QState, ProcessFC, TC.DeclMapFC)
typecheckEntry inp entry =
    case TC.inferWithDecls (OpsMap inp.inpCustomOps inp.inpCustomMeas) inp.inpDecls entry.deProc of
        Left errs -> Left (RpcError appErrorCode "typecheck error" (tcDiagnostics errs))
        Right (proc, decls)
            | entry.deRho.dim < minSize proc ->
                let msg = tooSmallMsg proc entry in Left (RpcError appErrorCode msg [JsonDiagnostic "error" msg Nothing])
            | otherwise -> Right (entry.deRho, proc, decls)

tooSmallMsg :: ProcessFC -> DistEntry -> String
tooSmallMsg proc entry =
    "quantum state smaller than required: expected >=" ++ show (minSize proc) ++ ", given " ++ show entry.deRho.dim

{- | Typecheck-only path for @check@: always replies with diagnostics (never an
RPC error), so live editing surfaces problems inline. Like the @check@ CLI
command, it checks every process declaration and every start block.
-}
checkSource :: Session -> Source -> IO (Either RpcError Result)
checkSource sess src = do
    er <- loadSource sess src
    pure $ Right $ RDiagnostics $ case er of
        Left (RpcError _ _ ds) -> ds
        Right inp -> case checkAll True inp of
            Left errs -> tcDiagnostics errs
            Right rs -> concatMap checkedDiagnostics rs

{- | File outline for the Sources panel: list each start block's name (the
unnamed default block is 'Nothing'), return the raw source for the editor
rail, and say which tag equivalences the file declares (so the GUI can default
the compare tag mode). When the file fails to parse, still return its raw text with no configs,
so the GUI loads it into the editor and surfaces the diagnostics via the live
linter instead of dropping the load entirely; only a source we cannot even read
propagates as an RPC error.
-}
outlineSource :: Session -> Source -> IO (Either RpcError Result)
outlineSource sess src = do
    er <- loadSource sess src
    case er of
        Right inp ->
            pure $ Right $
                ROutline
                    (map (.sbName) inp.inpStartBlocks)
                    inp.inpRaw
                    (not (null inp.inpTagEquiv))
                    [(c1, c2) | (c1, c2, es) <- inp.inpScopedTagEquiv, not (null es)]
        Left err -> do
            mraw <- rawSourceText src.sourceInput
            pure $ maybe (Left err) (\raw -> Right (ROutline [] raw False [])) mraw

{- | Best-effort raw text of a source, used on the outline error path so a file
that fails to parse still reaches the editor. 'Nothing' when the text cannot be
read at all (e.g. a missing file), in which case the original error is kept.
-}
rawSourceText :: SourceInput -> IO (Maybe String)
rawSourceText (SourceText t _) = pure (Just t)
rawSourceText (SourceFile f) = do
    r <- try (readUtf8 f)
    pure $ case r of
        Left (_ :: SomeException) -> Nothing
        Right s -> Just s

-- | Read a file as UTF-8, strictly (so the handle closes before returning).
readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    _ <- evaluate (length s)
    pure s

{- | Write the editor buffer back to @path@ as UTF-8 (independent of the ambient
locale, so `.quarc` sources round-trip). Any IO failure (bad path, permissions)
surfaces as an RPC error rather than crashing the sidecar.
-}
saveSource :: FilePath -> String -> IO (Either RpcError Result)
saveSource path text = do
    r <- try $ withFile path WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h text
    pure $ case r of
        Left (e :: SomeException) -> Left (RpcError appErrorCode ("could not save file: " ++ show e) [])
        Right () -> Right ROk

{- | Render the @eval@ or @minimize@ graph for a config to a static image via
graphviz (the publication-quality diagram, like @eval --format svg@). @svg@
is returned as text; @png@ is base64-encoded so it rides the text JSON-RPC
transport. Best-effort: a missing\/failing graphviz surfaces as an RPC error.
-}
exportGraph :: Report -> Session -> GVGenOptions -> Source -> Maybe String -> String -> String -> Equivalence -> Maybe Silence -> Maybe FilePath -> IO (Either RpcError Result)
exportGraph report sess opts src mconfig kind format equiv msilence mpath = do
    ep <- prepare report sess src mconfig
    case ep of
        Left e -> pure (Left e)
        Right (inp, rho, proc, decls) -> do
            report "evaluate" Nothing
            case evalEither (EV.eval decls (single rho proc)) of
                Left e -> pure (Left e)
                Right lts -> case graphFor inp lts of
                    Left e -> pure (Left (RpcError appErrorCode (bisimErrorMessage e) []))
                    Right dot -> do
                        report "render" (Just (HM.size lts.val.ltsStates))
                        r <- maybe (fmap (RExport format) <$> renderGraph format dot) (renderGraphTo format dot) mpath
                        pure $ either (\msg -> Left (RpcError appErrorCode msg [])) Right r
  where
    graphFor inp lts
        | kind == "minimize" = gvMinimal opts <$> minimalLTSUnder equiv (minimizeView inp msilence) lts
        | otherwise = Right (gv opts lts.val)

{- | Render a graph to the requested format's payload string (svg text, or
base64 png). Unknown formats fall back to svg.
-}
renderGraph :: String -> DotGraph Int -> IO (Either String String)
renderGraph "png" g = do
    r <- try $ graphvizWithHandle Dot g Png $ \h -> do
        bytes <- BS.hGetContents h -- strict: reads to EOF, safe for binary
        pure (BC.unpack (B64.encode bytes)) -- base64 is ASCII
    pure $ either (\(e :: SomeException) -> Left ("graphviz export failed: " ++ show e)) Right r
renderGraph _ g = do
    r <- try $ graphvizWithHandle Dot g Svg $ \h -> do
        s <- hGetContents h
        _ <- evaluate (length s) -- force before the handle closes
        pure s
    pure $ either (\(e :: SomeException) -> Left ("graphviz export failed: " ++ show e)) Right r

-- | Render a graph straight to a file (@png@ or, by default, @svg@).
renderGraphTo :: String -> DotGraph Int -> FilePath -> IO (Either String Result)
renderGraphTo format g path = do
    let (fmt, out) = if format == "png" then ("png", Png) else ("svg", Svg)
    r <- try (runGraphvizCommand Dot g out path)
    pure $ either (\(e :: SomeException) -> Left ("graphviz export failed: " ++ show e)) (Right . RExportFile fmt) r

-- | Per-config scoped tag equivalences for an @equiv@ pair (mirrors @Command.Equiv@).
scopedTagEquiv :: String -> String -> InputFileResult -> [(Tag, Tag)]
scopedTagEquiv c1 c2 inp = concat (scopedTo c1 c2 inp.inpScopedTagEquiv)

cacheStore :: Session -> GVGenOptions -> IntMap (QDist ProcessFC, DensityOp) -> IO Int
cacheStore sess opts idx =
    modifyMVar sess.store $ \(n, m) -> pure ((n + 1, IM.insert n (Cached opts idx) m), n)

doLayout :: Session -> DotGraph Int -> IO Layout
doLayout sess g
    | sessLayout sess = layoutDot g
    | otherwise = pure emptyLayout
