{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Per-phase pipeline benchmark: runs one task (check, eval, minimize or
compare) on one @.quarc@ file repeatedly and writes the raw per-phase
measurements as JSON. Graph layout and rendering are never invoked.

This executable measures a single task in a fresh process; the driver
@bench/run_benchmarks.py@ runs it once per task, aggregates the samples and
records the environment. Run it directly with:

> cabal run --enable-benchmarks pipeline-bench -- --file samples/cointoss.quarc --task eval --config protocol

Every repetition reruns the whole pipeline from the source text, so no phase
reuses work from a previous repetition. Each phase forces its result before
the clock stops; work a phase leaves lazy is attributed to the next phase that
demands it, which never changes the total.
-}
module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (replicateM_, unless, when)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.IntSet qualified as IS
import Data.LTS (LTS (..))
import Data.Maybe (listToMaybe)
import Data.WithData (getMeta, val)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Mem (getAllocationCounter, performMajorGC)

import LqCCS.Bisimulation qualified as Bisim
import LqCCS.EvalTagged qualified as EV
import LqCCS.Import (loadQuarcStringAs)
import LqCCS.Parser.File (DistEntry (..), InputFileResult (..), StartBlock (..), lookupStartBlock, scopedTo)
import LqCCS.QDist (QState (..), single)
import LqCCS.Quantum (minimalMeasures, minimalOps)
import LqCCS.Tagging (TagPolicy, applyTagPolicy, defaultTagPolicy, tagPolicyFromName, tagPolicyName)
import LqCCS.Typecheck (DeclMapFC, DeclResult (..), inferDeclarations, inferWith, inferWithDecls)
import LqCCS.Types (OpsMap (..), ProcessFC, minSize)

-- * Options

data Task = Check | Eval | Minimize | Compare
    deriving (Eq, Show, Enum, Bounded)

taskName :: Task -> String
taskName Check = "check"
taskName Eval = "eval"
taskName Minimize = "minimize"
taskName Compare = "compare"

data Opts = Opts
    { optFile :: !FilePath
    , optTask :: !Task
    , optConfigs :: ![String]
    , optTagging :: !TagPolicy
    , optTagMode :: !Bisim.TagMode
    , optEquivalence :: !Bisim.Equivalence
    , optRooting :: !Bisim.Rooting
    , optWarmup :: !Int
    , optMinReps :: !Int
    , optMaxReps :: !Int
    , optMinTime :: !Double
    , optOutput :: !(Maybe FilePath)
    }

optsParser :: Parser Opts
optsParser =
    Opts
        <$> strOption (long "file" <> metavar "PATH" <> help "The .quarc file")
        <*> option (maybeReader (\s -> lookup s [(taskName t, t) | t <- [minBound ..]])) (long "task" <> metavar "check|eval|minimize|compare" <> help "What to measure")
        <*> many (strOption (long "config" <> metavar "NAME" <> help "Start block (eval/minimize: one, compare: two)"))
        <*> option (maybeReader tagPolicyFromName) (long "tagging" <> value defaultTagPolicy <> showDefaultWith tagPolicyName <> help "Auto-tagging policy")
        <*> option (maybeReader Bisim.tagModeFromName) (long "tags" <> value Bisim.FileTags <> showDefaultWith Bisim.tagModeName <> help "file|demonic|angelic")
        <*> option (maybeReader Bisim.equivalenceFromName) (long "equivalence" <> value Bisim.Strong <> showDefaultWith Bisim.equivalenceName <> help "strong|branching")
        <*> flag Bisim.Unrooted Bisim.Rooted (long "rooted" <> help "Rooted branching bisimilarity (compare only)")
        <*> option auto (long "warmup" <> value 1 <> showDefault <> help "Unmeasured repetitions")
        <*> option auto (long "min-reps" <> value 5 <> showDefault <> help "Minimum measured repetitions")
        <*> option auto (long "max-reps" <> value 1000 <> showDefault <> help "Maximum measured repetitions")
        <*> option auto (long "min-time" <> value 1 <> showDefault <> help "Keep repeating until this many seconds were measured (bounded by --max-reps)")
        <*> optional (strOption (long "output" <> short 'o' <> metavar "PATH" <> help "Write the JSON here instead of stdout"))

-- * Measurement

-- | One timed phase: wall-clock nanoseconds, bytes allocated, GC wall-clock nanoseconds.
data PhaseSample = PhaseSample !Word64 !Int64 !Int64

{- | Run a phase and force its result with @force_it@ before the clock stops.
GC time is only available when the RTS collects statistics (@+RTS -T@).
-}
timed :: Bool -> (a -> ()) -> IO a -> IO (a, PhaseSample)
timed stats force_it act = do
    gc0 <- gcElapsed stats
    a0 <- getAllocationCounter
    t0 <- getMonotonicTimeNSec
    x <- act
    _ <- evaluate (force_it x)
    t1 <- getMonotonicTimeNSec
    a1 <- getAllocationCounter
    gc1 <- gcElapsed stats
    -- The allocation counter counts down.
    pure (x, PhaseSample (t1 - t0) (a0 - a1) (gc1 - gc0))

gcElapsed :: Bool -> IO Int64
gcElapsed False = pure 0
gcElapsed True = gc_elapsed_ns <$> getRTSStats

-- | A recorder for the phases of one repetition, in order.
type Phase = forall a. String -> (a -> ()) -> IO a -> IO a

-- | Summary of what a task computed; must be identical across repetitions.
type Outcome = [(K.Key, A.Value)]

-- * Forcing

forceLTS :: EV.DecSemantics meta -> ()
forceLTS lts =
    let l = lts.val
     in HM.size l.ltsStates `seq` HM.foldl' (\n acts -> n + length acts) (0 :: Int) l.ltsTransitions `seq` length l.ltsPredecessors `seq` ()

forceEither :: (b -> ()) -> Either a b -> ()
forceEither = either (`seq` ())

ltsSize :: EV.DecSemantics meta -> (Int, Int)
ltsSize lts = (HM.size lts.val.ltsStates, HM.foldl' (\n acts -> n + length acts) 0 lts.val.ltsTransitions)

{- | Peak resident set size of this process (@VmHWM@, Linux only). It is reset
by @exec@, so unlike the parent's @wait4@ it excludes the forking driver.
-}
peakRssBytes :: IO (Maybe Integer)
peakRssBytes = do
    status <- try @IOException (readFile "/proc/self/status")
    pure $ case status of
        Left _ -> Nothing
        Right txt -> listToMaybe [read kb * 1024 | ("VmHWM:" : kb : _) <- words <$> lines txt]

-- * Pipeline

dieWith :: String -> IO a
dieWith msg = hPutStrLn stderr ("pipeline-bench: " ++ msg) >> exitFailure

orDie :: Either String a -> IO a
orDie = either dieWith pure

parsePhase :: Phase -> FilePath -> String -> IO InputFileResult
parsePhase phase path contents = do
    r <-
        phase "parse" (forceEither (\i -> length i.inpDecls `seq` length i.inpStartBlocks `seq` ())) $
            loadQuarcStringAs path contents minimalOps minimalMeasures
    orDie (either (Left . ("parse error: " ++)) Right r)

taggingPhase :: Phase -> TagPolicy -> InputFileResult -> IO InputFileResult
taggingPhase phase policy inp =
    phase "tagging" (\i -> foldl' (flip seq) () i.inpDecls `seq` foldl' (\() b -> foldl' (\() e -> e.deProc `seq` ()) () b.sbEntries) () i.inpStartBlocks) $
        pure (applyTagPolicy policy inp)

opsOf :: InputFileResult -> OpsMap
opsOf inp = OpsMap inp.inpCustomOps inp.inpCustomMeas

entryOf :: InputFileResult -> String -> Either String DistEntry
entryOf inp name = maybe (Left ("no start block named " ++ show name)) Right (lookupStartBlock name inp >>= listToMaybe . sbEntries)

-- | Typecheck one start block the way the CLI does for eval/minimize/equiv.
typecheckEntry :: InputFileResult -> String -> Either String (QState, ProcessFC, DeclMapFC)
typecheckEntry inp name = do
    entry <- entryOf inp name
    (proc, decls) <- either (Left . (("typecheck of " ++ show name ++ " failed: ") ++) . show) Right (inferWithDecls (opsOf inp) inp.inpDecls entry.deProc)
    when (entry.deRho.dim < minSize proc) (Left ("quantum state of " ++ show name ++ " is too small"))
    pure (entry.deRho, proc, decls)

{- | Run one repetition of the task, recording each phase. Returns the outcome
summary used to check that repetitions agree.
-}
runTask :: Opts -> FilePath -> String -> Phase -> IO Outcome
runTask opts path contents phase = do
    parsed <- parsePhase phase path contents
    inp <- taggingPhase phase opts.optTagging parsed
    case (opts.optTask, opts.optConfigs) of
        (Check, []) -> do
            -- Mirrors the check command: every declaration, then every start block entry.
            (n_decls, n_ok, n_entries, n_entries_ok) <- phase "typecheck" (\(a, b, c, d) -> a `seq` b `seq` c `seq` d `seq` ()) $
                pure $
                    case inferDeclarations (opsOf inp) inp.inpDecls of
                        Left _ -> (0, 0, 0, 0)
                        Right (ctxt, decls) ->
                            let results = [fst (inferWith ctxt e.deProc) | b <- inp.inpStartBlocks, e <- b.sbEntries]
                             in (length decls, length [() | (_, DeclOk{}) <- decls], length results, length [() | Right _ <- results])
            pure ["declarations" .= (n_decls :: Int), "declarations_ok" .= (n_ok :: Int), "start_entries" .= (n_entries :: Int), "start_entries_ok" .= (n_entries_ok :: Int)]
        (t, [name]) | t `elem` [Eval, Minimize] -> do
            tc <- phase "typecheck" (forceEither (\(_, p, d) -> p `seq` d `seq` ())) (pure (typecheckEntry inp name))
            (rho, proc, decls) <- orDie tc
            ev <- phase "eval" (forceEither forceLTS) (pure (EV.eval decls (single rho proc)))
            lts <- orDie (either (Left . ("evaluation failed: " ++) . show) Right ev)
            let (n_states, n_trans) = ltsSize lts
                base = ["lts_states" .= n_states, "lts_transitions" .= n_trans]
            if t == Eval
                then pure base
                else do
                    let tv = Bisim.tagViewFor opts.optTagMode inp.inpTagEquiv inp.inpSilence
                        force_min (classes, mlts) = sum (IS.size <$> classes) `seq` HM.size mlts.ltsStates `seq` HM.foldl' (\n a -> n + length a) (0 :: Int) mlts.ltsTransitions `seq` ()
                    -- The quotient reuses the classes (the CLI computes them twice).
                    mr <-
                        phase "minimize" (forceEither force_min) $
                            pure ((\cs -> (cs, Bisim.quotientLTS opts.optEquivalence tv lts cs)) <$> Bisim.bisimilarityClassesUnder opts.optEquivalence tv lts)
                    (classes, mlts) <- orDie (either (Left . Bisim.bisimErrorMessage) Right mr)
                    pure (base ++ ["classes" .= length classes, "min_states" .= HM.size mlts.ltsStates, "min_transitions" .= HM.foldl' (\n a -> n + length a) (0 :: Int) mlts.ltsTransitions])
        (Compare, [c1, c2]) -> do
            tc <-
                phase "typecheck" (forceEither (\((_, p1, d1), (_, p2, d2)) -> p1 `seq` d1 `seq` p2 `seq` d2 `seq` ())) $
                    pure ((,) <$> typecheckEntry inp c1 <*> typecheckEntry inp c2)
            ((rho1, proc1, decls1), (rho2, proc2, decls2)) <- orDie tc
            ev <- phase "eval" (forceEither forceLTS) (pure (EV.evalPar [(decls1, single rho1 proc1), (decls2, single rho2 proc2)]))
            lts <- orDie (either (Left . ("evaluation failed: " ++) . show) Right ev)
            unless (getMeta @EV.NoFreeInputsKey lts) (dieWith "the processes have free quantum inputs; not checkable")
            (i1, i2) <- case lts.val.ltsInitial of
                [a, b] -> pure (a, b)
                is -> dieWith ("expected two initial states, got " ++ show (length is))
            let tag_equiv = inp.inpTagEquiv ++ concat (scopedTo c1 c2 inp.inpScopedTagEquiv)
                silence = inp.inpSilence <> mconcat (scopedTo c1 c2 inp.inpScopedSilence)
                tv = Bisim.tagViewFor opts.optTagMode tag_equiv silence
            vr <- phase "compare" (forceEither (`seq` ())) (pure (Bisim.checkBisimilar opts.optEquivalence opts.optRooting tv lts i1 i2))
            verdict <- orDie (either (Left . Bisim.bisimErrorMessage) Right vr)
            let (n_states, n_trans) = ltsSize lts
            pure ["lts_states" .= n_states, "lts_transitions" .= n_trans, "bisimilar" .= verdict]
        (t, cs) -> dieWith ("task " ++ taskName t ++ " does not take " ++ show (length cs) ++ " --config option(s)")

main :: IO ()
main = do
    opts <- execParser (info (optsParser <**> helper) (fullDesc <> progDesc "Measure the phases of one QUARC task (no layout or rendering)"))
    path <- makeAbsolute opts.optFile
    contents <- readFile path
    _ <- evaluate (length contents)
    stats <- getRTSStatsEnabled
    -- Phase names in first-seen order, and the samples of the current repetition.
    current <- newIORef []
    let phase :: Phase
        phase name force_it act = do
            (x, s) <- timed stats force_it act
            modifyIORef' current ((name, s) :)
            pure x
        one_rep = do
            performMajorGC
            modifyIORef' current (const [])
            outcome <- runTask opts path contents phase
            samples <- reverse <$> readIORef current
            pure (outcome, samples)
    -- The first repetition fixes the expected outcome; it counts as warmup unless there is none.
    first@(outcome, _) <- one_rep
    replicateM_ (opts.optWarmup - 1) one_rep
    let total (_, ss) = sum [t | (_, PhaseSample t _ _) <- ss]
        measure acc elapsed
            | n >= opts.optMaxReps = pure (reverse acc)
            | n >= opts.optMinReps && elapsed >= opts.optMinTime = pure (reverse acc)
            | otherwise = do
                r@(o, _) <- one_rep
                when (o /= outcome) (dieWith ("repetition " ++ show n ++ " computed a different result: " ++ show o))
                measure (r : acc) (elapsed + seconds r)
          where
            n = length acc
        seconds r = fromIntegral (total r) / 1e9 :: Double
    reps <- if opts.optWarmup > 0 then measure [] 0 else measure [first] (seconds first)
    rts <- if stats then Just <$> getRTSStats else pure Nothing
    peak_rss <- peakRssBytes
    let phase_names = maybe [] (map fst . snd) (listToMaybe reps)
        sample_json i (_, ss) =
            A.object
                [ "rep" .= (i :: Int)
                , "wall_ns" .= A.object [K.fromString n .= t | (n, PhaseSample t _ _) <- ss]
                , "alloc_bytes" .= A.object [K.fromString n .= a | (n, PhaseSample _ a _) <- ss]
                , "gc_ns" .= A.object [K.fromString n .= g | (n, PhaseSample _ _ g) <- ss]
                ]
        rts_json s =
            A.object
                [ "max_mem_in_use_bytes" .= s.max_mem_in_use_bytes
                , "max_live_bytes" .= s.max_live_bytes
                , "allocated_bytes" .= s.allocated_bytes
                , "gcs" .= s.gcs
                , "major_gcs" .= s.major_gcs
                , "gc_cpu_ns" .= s.gc_cpu_ns
                , "gc_elapsed_ns" .= s.gc_elapsed_ns
                , "mutator_cpu_ns" .= s.mutator_cpu_ns
                , "mutator_elapsed_ns" .= s.mutator_elapsed_ns
                ]
        doc =
            A.object
                [ "schema" .= ("quarc.bench.task/1" :: String)
                , "task" .= taskName opts.optTask
                , "file" .= opts.optFile
                , "configs" .= opts.optConfigs
                , "options"
                    .= A.object
                        [ "tagging" .= tagPolicyName opts.optTagging
                        , "tags" .= Bisim.tagModeName opts.optTagMode
                        , "equivalence" .= Bisim.equivalenceName opts.optEquivalence
                        , "rooted" .= (opts.optRooting == Bisim.Rooted)
                        , "warmup" .= opts.optWarmup
                        , "min_reps" .= opts.optMinReps
                        , "max_reps" .= opts.optMaxReps
                        , "min_time_s" .= opts.optMinTime
                        ]
                , "outcome" .= A.object outcome
                , "phases" .= phase_names
                , "samples" .= zipWith sample_json [0 ..] reps
                , "rts" .= maybe A.Null rts_json rts
                , "peak_rss_bytes" .= peak_rss
                ]
    maybe BL.putStr BL.writeFile opts.optOutput (A.encode doc)
