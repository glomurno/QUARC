{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : LqCCS.Bisimulation
Description : Strong and branching bisimilarity for lqCCS.

Implements strong and branching bisimilarity on 'DecSemantics' LTSs with a
single bottom-up signature pass that relies on the LTS being acyclic.

States are labelled by their quantum environment ('DensityOp' stored in the
@\"env\"@ metadata field): states with different environments are never
bisimilar. The pass visits states successors-first and gives each one a class
determined by its environment and its set of (action, successor class) pairs;
see 'strongBisimilarityNorm' and 'branchingBisimilarityNorm'. The partition
refinement fallback starts from the environment grouping and refines it with
the backward transition map until a fixpoint is reached.

== Tag equivalence

Users may supply a 'Tag' normalization function to identify action occurrences
that should be treated as the same observable step.
Both single-tag actions ('ActionTag') and synchronized-pair actions ('SynchTag')
may be placed in the same equivalence class, enabling cross-form matching (e.g.
@t1:tau ≡ (t2,t3):tau@).

== Branching bisimilarity

@tau@ steps ('ActionTau' and 'ActionTau2') whose normalized
tag is silent in the 'TagView' (every @tau@ tag by default, see 'Silence') have
their tag erased; the other @tau@ steps and all visible actions keep their
(normalized) tags. States are labelled by their environment,
so a @tau@ step that changes the environment is
never silent. The algorithm ('branchingBisimilarityNorm') visits states in
reverse topological order, so it requires an acyclic LTS and reports a
'CyclicLTS' error otherwise. It is the one-pass instance of the inductive
signatures of Blom & van de Pol (PDMC 2009, Def. 10) and
Martens & Laveaux (TACAS 2026, Def. 4).

== Precondition

The 'DecSemantics' must have its 'NoFreeInputsKV' flag set to 'True'
(i.e. no free receive of quantum values). Bisimilarity is not guaranteed to be
correct otherwise.
-}
module LqCCS.Bisimulation where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Either (fromRight)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable (..))
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.LTS
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.WithData
import GHC.Generics (Generic)

import LqCCS.EvalTagged
import LqCCS.Quantum qualified as Q
import LqCCS.Types (Channel, Expr, Silence (..), Tag (..))

-- | Integer identifier for a block in a 'Partition'.
type BlockId = Int

-- | A normalized action key used internally by the bisimulation algorithm.
data NormAction meta
    = NormTau !Tag
    | NormSend !Tag !Channel !(Expr meta)
    | NormRecvE !Tag !Channel !(Expr meta)
    deriving (Eq, Generic, Show)

instance Hashable (NormAction meta)

toNormAction :: (Tag -> Tag) -> Action meta -> NormAction meta
toNormAction norm (ActionTau t) = NormTau (norm (ActionTag t))
toNormAction norm (ActionTau2 t1 t2) = NormTau (norm (SynchTag t1 t2))
toNormAction norm (ActionSend t ch e) = NormSend (norm (ActionTag t)) ch e
toNormAction norm (ActionRecvE t ch e) = NormRecvE (norm (ActionTag t)) ch e

buildAdjMap :: (Hashable a) => [(a, a)] -> HashMap a [a]
buildAdjMap = foldl' (\m (a, b) -> HM.insertWith (++) a [b] (HM.insertWith (++) b [a] m)) HM.empty

connectedComponent :: (Hashable a) => a -> HashMap a [a] -> HashSet a
connectedComponent tag adj = go (HS.singleton tag) [tag]
  where
    go vis [] = vis
    go vis (t : ts) =
        let ns = filter (not . (`HS.member` vis)) (HM.lookupDefault [] t adj)
         in go (HS.union vis (HS.fromList ns)) (ns ++ ts)

buildTagNorm :: [(Tag, Tag)] -> (Tag -> Tag)
buildTagNorm pairs = \t -> HM.lookupDefault t t normMap
  where
    adj = buildAdjMap pairs
    allTags = HS.fromList (concatMap (\(a, b) -> [a, b]) pairs)
    (normMap, _) = foldl' processTag (HM.empty, allTags) (HS.toList allTags)
    processTag (m, remaining) t
        | not (HS.member t remaining) = (m, remaining)
        | otherwise =
            let cls = HS.toList (connectedComponent t adj)
                canon = minimum cls
             in ( foldl' (\m' c -> HM.insert c canon m') m cls
                , foldl' (flip HS.delete) remaining cls
                )

-- | Backward transition map.
type BwdMap meta = IM.IntMap (HM.HashMap (NormAction meta) IS.IntSet)

-- | A partition of state indices.
data Partition = Partition
    { ptS2B :: !(IM.IntMap BlockId)
    -- ^ Maps each state index to its current block identifier.
    , ptB2S :: !(IM.IntMap IS.IntSet)
    -- ^ Maps each block identifier to the set of state indices it contains.
    , ptNext :: !BlockId
    -- ^ The next unused block identifier (monotonically increasing).
    }

{- | A FIFO worklist of @(BlockId, NormAction)@ pairs.
The queue may accumulate stale entries (whose block has since been
retired by a split). Membership in 'wlSet' is the source of truth.
-}
data Worklist meta = Worklist
    { wlQueue :: !(Seq.Seq (BlockId, NormAction meta))
    -- ^ FIFO queue of pending @(block, action)@ splitter candidates.
    , wlSet :: !(HS.HashSet (BlockId, NormAction meta))
    -- ^ Set of live entries.
    }

mkPartition :: [[Int]] -> Partition
mkPartition groups =
    let indexed = zip [0 ..] groups
     in Partition
            { ptS2B = IM.fromList [(s, i) | (i, g) <- indexed, s <- g]
            , ptB2S = IM.fromList [(i, IS.fromList g) | (i, g) <- indexed]
            , ptNext = length groups
            }

singleBlock :: [Int] -> Partition
singleBlock block = mkPartition [block]

doSplit :: BlockId -> IS.IntSet -> IS.IntSet -> Partition -> (Partition, BlockId, BlockId)
doSplit old c1 c2 pt =
    let b1 = ptNext pt
        b2 = b1 + 1
        s2b' = IS.foldl' (\m s -> IM.insert s b1 m) (ptS2B pt) c1
        s2b'' = IS.foldl' (\m s -> IM.insert s b2 m) s2b' c2
        b2s' = IM.insert b1 c1 . IM.insert b2 c2 . IM.delete old $ ptB2S pt
     in (Partition s2b'' b2s' (b2 + 1), b1, b2)

wlFromList :: [(BlockId, NormAction meta)] -> Worklist meta
wlFromList xs = Worklist (Seq.fromList xs) (HS.fromList xs)

wlPop :: Worklist meta -> Maybe ((BlockId, NormAction meta), Worklist meta)
wlPop wl = case Seq.viewl (wlQueue wl) of
    Seq.EmptyL -> Nothing
    x Seq.:< xs ->
        let wl' = wl{wlQueue = xs}
         in if HS.member x (wlSet wl)
                then Just (x, wl'{wlSet = HS.delete x (wlSet wl')})
                else wlPop wl'

wlPush :: (BlockId, NormAction meta) -> Worklist meta -> Worklist meta
wlPush x (Worklist queue set)
    | HS.member x set = Worklist queue set
    | otherwise = Worklist (queue Seq.|> x) (HS.insert x set)

wlRemove :: (BlockId, NormAction meta) -> Worklist meta -> Worklist meta
wlRemove x wl = wl{wlSet = HS.delete x (wlSet wl)}

buildBwdMapWith ::
    (Action meta -> NormAction meta) ->
    HM.HashMap (Int, Int) (HS.HashSet (Action meta)) ->
    BwdMap meta
buildBwdMapWith toNorm = HM.foldlWithKey' addEdges IM.empty
  where
    addEdges acc (src, tgt) =
        HS.foldl'
            (\m a -> IM.alter (Just . HM.insertWith IS.union (toNorm a) (IS.singleton src) . fromMaybe HM.empty) tgt m)
            acc

buildBwdMap :: HM.HashMap (Int, Int) (HS.HashSet (Action meta)) -> BwdMap meta
buildBwdMap = buildBwdMapWith (toNormAction id)

buildBwdMapNorm :: HM.HashMap (Int, Int) (HS.HashSet (NormAction meta)) -> BwdMap meta
buildBwdMapNorm = HM.foldlWithKey' addEdges IM.empty
  where
    addEdges acc (src, tgt) =
        HS.foldl'
            (\m a -> IM.alter (Just . HM.insertWith IS.union a (IS.singleton src) . fromMaybe HM.empty) tgt m)
            acc

predecessorsVia :: BwdMap meta -> NormAction meta -> IS.IntSet -> IS.IntSet
predecessorsVia bwd a = IS.foldl' (\acc t -> IS.union acc (fromMaybe IS.empty (IM.lookup t bwd >>= HM.lookup a))) IS.empty

strongBisim :: BwdMap meta -> [NormAction meta] -> Partition -> Partition
strongBisim bwd acts pt0 = go wl0 pt0
  where
    wl0 = wlFromList [(b, a) | b <- IM.keys (ptB2S pt0), a <- acts]

    go wl pt = case wlPop wl of
        Nothing -> pt
        Just ((bid, a), wl') ->
            case IM.lookup bid (ptB2S pt) of
                Nothing -> go wl' pt
                Just splitter ->
                    let pre = predecessorsVia bwd a splitter
                        (pt', wl'') = refineAll pre pt wl'
                     in go wl'' pt'

    refineAll pre pt wl =
        let candidates = IS.foldl' (\acc s -> maybe acc (`IS.insert` acc) (IM.lookup s (ptS2B pt))) IS.empty pre
         in foldl' (refineOne pre) (pt, wl) (IS.toList candidates)

    refineOne pre (pt, wl) cid = case IM.lookup cid (ptB2S pt) of
        Nothing -> (pt, wl)
        Just cstates ->
            let c1 = IS.intersection cstates pre
                c2 = IS.difference cstates pre
             in if IS.null c1 || IS.null c2
                    then (pt, wl)
                    else
                        let (pt', b1, b2) = doSplit cid c1 c2 pt
                            wl' = foldl' (updateWl cid b1 b2) wl acts
                         in (pt', wl')

    -- TODO: enforce deterministic tagging and switch to queueing only the smaller one
    updateWl cid b1 b2 wl a = wlPush (b2, a) . wlPush (b1, a) . wlRemove (cid, a) $ wl

stateIndices :: DecSemantics meta -> [Int]
stateIndices lts = [idx.val | idx <- HM.elems lts.val.ltsStates]

alphabetWith :: (Action meta -> NormAction meta) -> DecSemantics meta -> [NormAction meta]
alphabetWith toNorm =
    HS.toList
        . HM.foldl' (HS.foldl' (\s a -> HS.insert (toNorm a) s)) HS.empty
        . ltsTransitions
        . val

alphabet :: DecSemantics meta -> [NormAction meta]
alphabet = alphabetWith (toNormAction id)

groupByEnv :: DecSemantics meta -> [[Int]]
groupByEnv lts =
    HM.elems $
        HM.fromListWith
            (++)
            [ (Q.canonicalDensityKey Q.defaultApproxEpsilon (getEnv s), [s.val])
            | s <- HM.elems lts.val.ltsStates
            ]

{- | Strong bisimilarity by partition refinement under tag normalization @norm@,
starting from a given initial grouping of state indices. Works on any LTS,
cyclic or not. Returns a map from state index to block id.
-}
strongBisimilarityRefine :: (Tag -> Tag) -> DecSemantics meta -> [[Int]] -> IM.IntMap BlockId
strongBisimilarityRefine norm lts groups =
    let proj = toNormAction norm
     in ptS2B $ strongBisim (buildBwdMapWith proj lts.val.ltsTransitions) (alphabetWith proj lts) (mkPartition groups)

{- | Strong bisimilarity under tag normalization @norm@, with
states grouped by environment. Returns a map from state index to class id.

On an acyclic LTS this is the one-pass signature algorithm of
'branchingBisimilarityNorm' with no silent actions.
-}
strongBisimilarityNorm :: (Tag -> Tag) -> DecSemantics meta -> IM.IntMap BlockId
strongBisimilarityNorm norm lts =
    fromRight (strongBisimilarityRefine norm lts (groupByEnv lts)) $
        branchingBisimilarityNorm (TagView norm (const False)) lts

{- | Strong bisimilarity partition by partition refinement,
starting from a given initial grouping of state indices. Returns a map from
state index to block id.
-}
strongBisimilarityWith :: DecSemantics meta -> [[Int]] -> IM.IntMap BlockId
strongBisimilarityWith = strongBisimilarityRefine id

{- | Strong bisimilarity partition, with states grouped by
environment. Returns a map from state index to block id.
-}
strongBisimilarity :: DecSemantics meta -> IM.IntMap BlockId
strongBisimilarity = strongBisimilarityNorm id

{- | Returns 'True' if states @s@ and @t@ are strongly bisimilar in the LTS,
using the given tag normalization function.
-}
strongBisimilarWith :: (Tag -> Tag) -> DecSemantics meta -> Int -> Int -> Bool
strongBisimilarWith norm lts s t =
    let part = strongBisimilarityNorm norm lts
     in case (IM.lookup s part, IM.lookup t part) of
            (Just bs, Just bt) -> bs == bt
            _ -> False

-- | Returns 'True' if states @s@ and @t@ are strongly bisimilar in the LTS.
strongBisimilar :: DecSemantics meta -> Int -> Int -> Bool
strongBisimilar = strongBisimilarWith id

{- | Return the list of equivalence classes under strong bisimilarity,
using the given tag normalization.
-}
strongBisimilarityClassesWith :: (Tag -> Tag) -> DecSemantics meta -> [IS.IntSet]
strongBisimilarityClassesWith norm lts = classesOf (strongBisimilarityNorm norm lts)

-- | Return the list of equivalence classes under strong bisimilarity.
strongBisimilarityClasses :: DecSemantics meta -> [IS.IntSet]
strongBisimilarityClasses = strongBisimilarityClassesWith id

-- | Which behavioural equivalence to compute.
data Equivalence
    = Strong
    | Branching
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Errors raised by the bisimilarity computations.
newtype BisimError
    = -- | The LTS has a cycle through the given state; branching bisimilarity requires an acyclic LTS.
      CyclicLTS Int
    deriving (Eq, Show)

{- | Whether an equivalence check between two states also imposes the root
condition: every initial step, @tau@ included, must be matched by a step with
the same label into an equivalent state
-}
data Rooting = Unrooted | Rooted
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | A human-readable description of a 'BisimError'.
bisimErrorMessage :: BisimError -> String
bisimErrorMessage (CyclicLTS s) =
    "the transition system has a cycle through state " ++ show s ++ "; branching bisimilarity requires an acyclic transition system"

equivalenceName :: Equivalence -> String
equivalenceName Strong = "strong"
equivalenceName Branching = "branching"

equivalenceFromName :: String -> Maybe Equivalence
equivalenceFromName "strong" = Just Strong
equivalenceFromName "branching" = Just Branching
equivalenceFromName _ = Nothing

-- | How action tags are observed by the bisimilarity checks.
data TagView = TagView
    { tvNorm :: Tag -> Tag
    -- ^ Tag normalization, as built by 'buildTagNorm'.
    , tvSilent :: Tag -> Bool
    -- ^ Whether a normalized @tau@ tag is silent.
    }

-- | Build a 'TagView' from tag-equivalence pairs and a 'Silence' declaration.
tagView :: [(Tag, Tag)] -> Silence -> TagView
tagView pairs = tagViewWith (buildTagNorm pairs)

-- | Like 'tagView', for an arbitrary tag normalization.
tagViewWith :: (Tag -> Tag) -> Silence -> TagView
tagViewWith norm (Silence silent visible) = TagView norm isSilentTag
  where
    silentSet = HS.fromList (norm <$> silent)
    visibleSet = HS.fromList (norm <$> visible)
    isSilentTag t = (null silent || HS.member t silentSet) && not (HS.member t visibleSet)

-- | How tags are identified by a bisimilarity check.
data TagMode
    = -- | The tag equivalences declared by the file (and the command line).
      FileTags
    | -- | No tags are identified (the empty relation, "demonic").
      DemonicTags
    | -- | Every tag is equivalent to every other (the full relation, "angelic").
      AngelicTags
    deriving (Eq, Show)

tagModeName :: TagMode -> String
tagModeName FileTags = "file"
tagModeName DemonicTags = "demonic"
tagModeName AngelicTags = "angelic"

tagModeFromName :: String -> Maybe TagMode
tagModeFromName "file" = Just FileTags
tagModeFromName "demonic" = Just DemonicTags
tagModeFromName "angelic" = Just AngelicTags
tagModeFromName _ = Nothing

tagViewFor :: TagMode -> [(Tag, Tag)] -> Silence -> TagView
tagViewFor FileTags pairs = tagView pairs
tagViewFor DemonicTags _ = tagView []
tagViewFor AngelicTags _ = tagViewWith (const (ActionTag 0))

-- | A 'TagView' with the given normalization in which every @tau@ tag is silent.
normView :: (Tag -> Tag) -> TagView
normView norm = TagView norm (const True)

-- | Whether a normalized action is a silent step under 'Branching'.
isSilent :: TagView -> NormAction meta -> Bool
isSilent tv (NormTau t) = tvSilent tv t
isSilent _ _ = False

{- | A signature entry: the observable part of an action ('Nothing' for a silent
step, whose tag is erased) paired with the class of its target.
-}
type SigEntry meta = (Maybe (NormAction meta), BlockId)

sigLabel :: TagView -> NormAction meta -> Maybe (NormAction meta)
sigLabel tv a = if isSilent tv a then Nothing else Just a

type DensityKey = (Int, Int, [(Integer, Integer)])

type FwdMap meta = IM.IntMap [(NormAction meta, Int)]

buildFwdMapWith ::
    (Action meta -> NormAction meta) ->
    HM.HashMap (Int, Int) (HS.HashSet (Action meta)) ->
    FwdMap meta
buildFwdMapWith toNorm = HM.foldlWithKey' add IM.empty
  where
    add acc (src, tgt) acts = IM.insertWith (++) src [(toNorm a, tgt) | a <- HS.toList acts] acc

data Mark = Active | Done

reverseTopoOrder :: FwdMap meta -> [Int] -> Either BisimError [Int]
reverseTopoOrder fwd states = reverse . snd <$> foldM visit (IM.empty, []) states
  where
    visit (marks, acc) s = case IM.lookup s marks of
        Just Done -> Right (marks, acc)
        Just Active -> Left (CyclicLTS s)
        Nothing -> do
            (marks', acc') <- foldM visit (IM.insert s Active marks, acc) (snd <$> IM.findWithDefault [] s fwd)
            Right (IM.insert s Done marks', s : acc')

-- | State of the bottom-up pass of 'branchingBisimilarityNorm'.
data BranchState meta = BranchState
    { bsClass :: !(IM.IntMap BlockId)
    -- ^ Class of every state processed so far.
    , bsSig :: !(IM.IntMap (HS.HashSet (SigEntry meta)))
    -- ^ Signature of each class: the local signature of its bottom states.
    , bsTable :: !(HM.HashMap (DensityKey, HS.HashSet (SigEntry meta)) BlockId)
    -- ^ Class of each (environment, signature) pair seen at a bottom state.
    , bsNext :: !BlockId
    -- ^ The next unused class identifier.
    }

newBS :: Int -> HashSet (SigEntry meta) -> DensityKey -> BranchState meta -> BranchState meta
newBS s pre envS st =
    BranchState
        { bsClass = IM.insert s st.bsNext st.bsClass
        , bsSig = IM.insert st.bsNext pre st.bsSig
        , bsTable = HM.insert (envS, pre) st.bsNext st.bsTable
        , bsNext = st.bsNext + 1
        }

assignBS :: Int -> BlockId -> BranchState meta -> BranchState meta
assignBS s k st = st{bsClass = IM.insert s k st.bsClass}

{- | Branching bisimilarity under the tag view @tv@.
Returns a map from state index to class id.
-}
branchingBisimilarityNorm :: TagView -> DecSemantics meta -> Either BisimError (IM.IntMap BlockId)
branchingBisimilarityNorm tv lts = do
    order <- reverseTopoOrder fwd (IM.keys envs)
    pure $ bsClass $ foldl' visitState (BranchState IM.empty IM.empty HM.empty 0) order
  where
    fwd = buildFwdMapWith (toNormAction tv.tvNorm) lts.val.ltsTransitions
    envs = envKeys lts
    envOf s = envs IM.! s

    visitState st s =
        let classOf t = st.bsClass IM.! t
            sigOf k = st.bsSig IM.! k
            outs = IM.findWithDefault [] s fwd
            pre = HS.fromList [(sigLabel tv a, classOf t) | (a, t) <- outs]
            envS = envOf s
            joinable t = envOf t == envS && all (\e -> e == (Nothing, classOf t) || e `HS.member` sigOf (classOf t)) pre
            joinClasses = [classOf t | (a, t) <- outs, isSilent tv a, joinable t]
         in maybe (newBS s pre envS st) (\k -> assignBS s k st) $ (fst <$> uncons joinClasses) <|> HM.lookup (envS, pre) st.bsTable

-- | The bucketed environment key of every state.
envKeys :: DecSemantics meta -> IM.IntMap DensityKey
envKeys lts = IM.fromList [(s.val, Q.canonicalDensityKey Q.defaultApproxEpsilon (getEnv s)) | s <- HM.elems lts.val.ltsStates]

-- | Group a state-to-class map into its classes, ordered by class id.
classesOf :: IM.IntMap BlockId -> [IS.IntSet]
classesOf part = IM.elems (IM.fromListWith IS.union [(b, IS.singleton s) | (s, b) <- IM.toList part])

{- | Return the equivalence classes under branching bisimilarity, using the given
tag normalization.
-}
branchingBisimilarityClassesWith :: TagView -> DecSemantics meta -> Either BisimError [IS.IntSet]
branchingBisimilarityClassesWith tv lts = classesOf <$> branchingBisimilarityNorm tv lts

{- | Returns whether states @s@ and @t@ are branching bisimilar in the LTS, using
the given tag normalization.
-}
branchingBisimilarWith :: TagView -> DecSemantics meta -> Int -> Int -> Either BisimError Bool
branchingBisimilarWith tv lts s t = do
    part <- branchingBisimilarityNorm tv lts
    pure $ case (IM.lookup s part, IM.lookup t part) of
        (Just bs, Just bt) -> bs == bt
        _ -> False

{- | Returns whether states @s@ and @t@ are rooted branching bisimilar, using the
given tag normalization.
-}
rootedBranchingBisimilarWith :: TagView -> DecSemantics meta -> Int -> Int -> Either BisimError Bool
rootedBranchingBisimilarWith tv lts s t = do
    part <- branchingBisimilarityNorm tv lts
    let fwd = buildFwdMapWith (toNormAction (tvNorm tv)) lts.val.ltsTransitions
        envOf = envKeys lts
        rootSig u = HS.fromList <$> traverse (\(a, v) -> (sigLabel tv a,) <$> IM.lookup v part) (IM.findWithDefault [] u fwd)
    pure $ case (IM.lookup s envOf, IM.lookup t envOf, rootSig s, rootSig t) of
        (Just es, Just et, Just rs, Just rt) -> es == et && rs == rt
        _ -> False

-- | Equivalence classes under the given 'Equivalence'.
bisimilarityClassesUnder :: Equivalence -> TagView -> DecSemantics meta -> Either BisimError [IS.IntSet]
bisimilarityClassesUnder Strong tv lts = Right (strongBisimilarityClassesWith (tvNorm tv) lts)
bisimilarityClassesUnder Branching tv lts = branchingBisimilarityClassesWith tv lts

-- | Whether two states are equivalent under the given 'Equivalence'.
bisimilarUnder :: Equivalence -> TagView -> DecSemantics meta -> Int -> Int -> Either BisimError Bool
bisimilarUnder Strong tv lts s t = Right (strongBisimilarWith (tvNorm tv) lts s t)
bisimilarUnder Branching tv lts s t = branchingBisimilarWith tv lts s t

-- | Whether two states are equivalent under the given 'Equivalence' and 'Rooting'.
checkBisimilar :: Equivalence -> Rooting -> TagView -> DecSemantics meta -> Int -> Int -> Either BisimError Bool
checkBisimilar Branching Rooted tv lts s t = rootedBranchingBisimilarWith tv lts s t
-- strong bisimilarity already satisfies the root condition
checkBisimilar equiv _ tv lts s t = bisimilarUnder equiv tv lts s t

checkName :: Equivalence -> Rooting -> String
checkName Branching Rooted = "rooted branching"
checkName equiv _ = equivalenceName equiv

{- | Minimized LTS: states are the bisimilarity equivalence
classes, represented as lists of original state indices. Each state carries
the environment 'DensityOp' of a representative element of its class.
Transitions are labeled with 'NormAction' keys under the given normalization.
-}
type MinimalSemantics meta = LTS [Int] '[EnvKV] HashSet (NormAction meta)

{- | Quotient a 'DecSemantics' by the given equivalence classes. Transitions
between classes are inferred from transitions between their members, with
actions projected via 'toNormAction'. Under 'Branching', silent steps within a
class are inert and dropped.
-}
quotientLTS :: Equivalence -> TagView -> DecSemantics meta -> [IS.IntSet] -> MinimalSemantics meta
quotientLTS equiv tv lts classList =
    let normA = toNormAction (tvNorm tv)
        envMap = IM.fromList [(s.val, getEnv s) | s <- HM.elems lts.val.ltsStates]
        classes = zip classList [0 ..]
        revClasses = IM.fromList (concat [(,idx) <$> IS.toList cls | (cls, idx) <- classes])
        states = HM.fromList [(IS.toList cls, WD @'[EnvKV] (Cons (EnvBox (envMap IM.! IS.findMin cls)) Nil) idx) | (cls, idx) <- classes]
        inert cs ct a = equiv == Branching && cs == ct && isSilent tv a
        trans =
            HM.filter (not . HS.null) $
                HM.fromListWith
                    HS.union
                    [ ((cs, ct), HS.filter (not . inert cs ct) (HS.map normA acts))
                    | ((s, t), acts) <- HM.toList lts.val.ltsTransitions
                    , let cs = revClasses IM.! s
                    , let ct = revClasses IM.! t
                    ]
        inits = (revClasses IM.!) <$> lts.val.ltsInitial
        roots = (revClasses IM.!) <$> lts.val.ltsRoot
     in LTS states trans (buildBwdMapNorm trans) inits roots

-- | Compute the minimized LTS by strong bisimilarity under tag normalization @norm@.
minimalLTSWith :: (Tag -> Tag) -> DecSemantics meta -> MinimalSemantics meta
minimalLTSWith norm lts = quotientLTS Strong (normView norm) lts (strongBisimilarityClassesWith norm lts)

-- | Compute the minimized LTS by strong bisimilarity.
minimalLTS :: DecSemantics meta -> MinimalSemantics meta
minimalLTS = minimalLTSWith id

-- | Compute the minimized LTS by branching bisimilarity under tag view @tv@.
branchingMinimalLTSWith :: TagView -> DecSemantics meta -> Either BisimError (MinimalSemantics meta)
branchingMinimalLTSWith tv lts = quotientLTS Branching tv lts <$> branchingBisimilarityClassesWith tv lts

-- | The minimal quotient under the given 'Equivalence'.
minimalLTSUnder :: Equivalence -> TagView -> DecSemantics meta -> Either BisimError (MinimalSemantics meta)
minimalLTSUnder Strong tv lts = Right (minimalLTSWith (tvNorm tv) lts)
minimalLTSUnder Branching tv lts = branchingMinimalLTSWith tv lts
