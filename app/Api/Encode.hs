{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Api.Encode
Description : Build @quarc.lts\/1@ documents from evaluated LTSs.
-}
module Api.Encode (
    Layout,
    buildStateIndex,
    emptyLayout,
    equivDocument,
    equivDocumentSummaryWith,
    equivDocumentWith,
    equivMinimizedDocumentWith,
    equivQuotient,
    evalDocument,
    evalDocumentSummaryWith,
    evalDocumentWith,
    minimizeDocument,
    minimizeDocumentWith,
    nodeDetailAt,
) where

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.LTS (LTS (..))
import Data.List (intercalate, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.WithData (WithData (..), getMeta)
import Numeric.LinearAlgebra (imagPart, realPart, toLists)
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (Unbounded), Pretty (pretty), layoutPretty)
import Prettyprinter.Render.String (renderString)

import Api.Contract
import Api.Layout (Pos, layoutDot)
import LqCCS.Bisimulation (
    BisimError,
    Equivalence,
    MinimalSemantics,
    NormAction (..),
    Rooting,
    TagView (..),
    bisimilarityClassesUnder,
    checkBisimilar,
    minimalLTSUnder,
    quotientLTS,
    toNormAction,
 )
import LqCCS.EvalTagged (
    Action (..),
    DecSemanticsFC,
    NoFreeInputsKey,
    Semantics,
    getEnv,
    simplify,
 )
import LqCCS.Pretty (GVGenOptions (..), gv, gvMinimal, prettyProcess, startPrec)
import LqCCS.QDist (QDist (..), QState (..), SubState (..))
import LqCCS.Quantum qualified as Q
import LqCCS.Types

render1 :: Doc ann -> String
render1 = renderString . layoutPretty (LayoutOptions Unbounded)

prettyProc :: GVGenOptions -> ProcessFC -> String
prettyProc opts p =
    render1 $ prettyProcess opts.gvPrettyOptions startPrec (if opts.simplifyLTS then simplify p else p)

procShortOf :: GVGenOptions -> QDist ProcessFC -> String
procShortOf opts (QDist (SubState _ p : _)) = prettyProc opts p
procShortOf _ _ = "ε"

hilbertDim :: QState -> Integer
hilbertDim qs = 2 ^ qs.dim

qdistDim :: QDist a -> Integer
qdistDim (QDist (SubState qs _ : _)) = hilbertDim qs
qdistDim _ = 0

branchesOf :: QDist a -> Int
branchesOf (QDist subs) = length subs

nodeLocOf :: QDist ProcessFC -> Maybe SourceSpan
nodeLocOf (QDist (SubState _ p : _)) = fcToSpan (getLoc p)
nodeLocOf _ = Nothing

matrixOf :: DensityOp -> ComplexMatrix
matrixOf (DensityOp m) =
    let rows = toLists m
        nr = length rows
        nc = case rows of r : _ -> length r; [] -> 0
     in ComplexMatrix nr nc [[realPart z, imagPart z] | row <- rows, z <- row]

detailOf :: GVGenOptions -> QDist ProcessFC -> DensityOp -> NodeDetail
detailOf opts (QDist subs) envOp =
    NodeDetail
        (matrixOf envOp)
        [ SubStateJ (realPart (Q.trace qs.rho)) (hilbertDim qs) (matrixOf qs.rho) (prettyProc opts p)
        | SubState qs p <- subs
        ]

fcToSpan :: FC -> Maybe SourceSpan
fcToSpan NoFC = Nothing
fcToSpan (FC f (sl, sc) (el, ec)) = Just (SourceSpan f sl sc el ec)

tagLocsOf :: ProcessFC -> [(Integer, FC)]
tagLocsOf (WD _ p) = case p of
    Discard _ -> []
    Tau t k -> (t.val, getLoc t) : tagLocsOf k
    Tau2 t1 t2 k -> (t1.val, getLoc t1) : (t2.val, getLoc t2) : tagLocsOf k
    Send t _ _ k -> (t.val, getLoc t) : tagLocsOf k
    Recv t _ _ k -> (t.val, getLoc t) : tagLocsOf k
    Apply t _ _ k -> (t.val, getLoc t) : tagLocsOf k
    Meas t _ _ _ k -> (t.val, getLoc t) : tagLocsOf k
    ITE _ a b -> tagLocsOf a ++ tagLocsOf b
    Sum ps -> concatMap tagLocsOf ps
    Par ps -> concatMap tagLocsOf ps
    Restr k _ -> tagLocsOf k
    Call{} -> []

buildTagLocs :: Semantics '[LocKV] -> Map Integer FC
buildTagLocs lts =
    Map.fromList
        [ (tg, fc)
        | (QDist subs, _) <- HM.toList lts.ltsStates
        , SubState _ p <- subs
        , (tg, fc) <- tagLocsOf p
        , fc /= NoFC
        ]

spanFor :: Map Integer FC -> Integer -> Maybe SourceSpan
spanFor tm t = Map.lookup t tm >>= fcToSpan

chanOf :: Channel -> Chan
chanOf (Channel n ty) = Chan n (render1 (pretty ty))

actionLabel :: Map Integer FC -> Action '[LocKV] -> ActionLabel
actionLabel tm a = case a of
    ActionTau t -> ALTau t lbl (spanFor tm t)
    ActionTau2 s r -> ALTau2 s r lbl (spanFor tm s)
    ActionSend t ch e -> ALSend t (chanOf ch) (render1 (pretty e)) lbl (spanFor tm t)
    ActionRecvE t ch e -> ALRecv t (chanOf ch) (render1 (pretty e)) lbl (spanFor tm t)
  where
    lbl = render1 (pretty a)

tagInt :: Tag -> Integer
tagInt (ActionTag n) = n
tagInt (SynchTag s _) = s

normActionLabel :: Map Integer FC -> NormAction '[LocKV] -> ActionLabel
normActionLabel tm a = case a of
    NormTau (ActionTag n) -> ALTau n lbl (spanFor tm n)
    NormTau (SynchTag s r) -> ALTau2 s r lbl (spanFor tm s)
    NormSend t ch e -> ALSend (tagInt t) (chanOf ch) (render1 (pretty e)) lbl (spanFor tm (tagInt t))
    NormRecvE t ch e -> ALRecv (tagInt t) (chanOf ch) (render1 (pretty e)) lbl (spanFor tm (tagInt t))
  where
    lbl = render1 (pretty a)

tagRef :: Tag -> TagRef
tagRef (ActionTag n) = TagOne n
tagRef (SynchTag a b) = TagPair a b

tauTagsOf :: TagView -> Map Integer FC -> Semantics '[LocKV] -> [TauTagInfo]
tauTagsOf tv tm sem =
    [ TauTagInfo (tagRef t) (tagRef n) (tvSilent tv n) (spanFor tm (tagInt t))
    | t <- Set.toList tags
    , let n = tvNorm tv t
    ]
  where
    tags = Set.fromList [t | acts <- HM.elems sem.ltsTransitions, a <- HS.toList acts, Just t <- [tauTag a]]
    tauTag (ActionTau n) = Just (ActionTag n)
    tauTag (ActionTau2 a b) = Just (SynchTag a b)
    tauTag _ = Nothing

type Layout = (IntMap Pos, Map (Int, Int) [Pos])

emptyLayout :: Layout
emptyLayout = (IM.empty, Map.empty)

evalDocumentWith :: GVGenOptions -> Layout -> DecSemanticsFC -> LtsDocument
evalDocumentWith opts layout lts = evalDoc opts layout lts (detailMap opts lts.val)

evalDocumentSummaryWith :: GVGenOptions -> Layout -> DecSemanticsFC -> LtsDocument
evalDocumentSummaryWith opts layout lts = evalDoc opts layout lts []

evalDoc :: GVGenOptions -> Layout -> DecSemanticsFC -> [(Int, NodeDetail)] -> LtsDocument
evalDoc opts (posMap, bendsMap) lts detail = LtsDocument KEval graph Nothing Nothing Nothing (IM.toList posMap) detail Nothing
  where
    sem = lts.val
    tm = buildTagLocs sem
    nodes =
        [ NodeSummary s.val (qdistDim qd) (branchesOf qd) (procShortOf opts qd) (s.val `elem` sem.ltsInitial) Nothing Nothing (nodeLocOf qd)
        | (qd, s) <- HM.toList sem.ltsStates
        ]
    graph = Graph sem.ltsInitial (getMeta @NoFreeInputsKey lts) nodes (edgesOf id tm bendsMap sem)

evalDocument :: GVGenOptions -> DecSemanticsFC -> IO LtsDocument
evalDocument opts lts = do
    l <- layoutDot (gv opts{onlyDots = True} lts.val)
    pure (evalDocumentWith opts l lts)

equivDocumentWith :: GVGenOptions -> Equivalence -> Rooting -> TagView -> String -> Layout -> DecSemanticsFC -> Either BisimError LtsDocument
equivDocumentWith opts equiv rooting tv bisimType layout lts = equivDoc opts equiv rooting tv bisimType layout lts (detailMap opts lts.val)

equivDocumentSummaryWith :: GVGenOptions -> Equivalence -> Rooting -> TagView -> String -> Layout -> DecSemanticsFC -> Either BisimError LtsDocument
equivDocumentSummaryWith opts equiv rooting tv bisimType layout lts = equivDoc opts equiv rooting tv bisimType layout lts []

equivDoc :: GVGenOptions -> Equivalence -> Rooting -> TagView -> String -> Layout -> DecSemanticsFC -> [(Int, NodeDetail)] -> Either BisimError LtsDocument
equivDoc opts equiv rooting tv bisimType (posMap, bendsMap) lts detail = do
    classesIS <- bisimilarityClassesUnder equiv tv lts
    bisim <- case sem.ltsInitial of
        [a, b] -> Just <$> checkBisimilar equiv rooting tv lts a b
        _ -> Right Nothing
    let classIdOf = IM.fromList [(i, ci) | (cls, ci) <- zip classesIS [0 :: Int ..], i <- IS.toList cls]
        nodes =
            [ NodeSummary s.val (qdistDim qd) (branchesOf qd) (procShortOf opts qd) (s.val `elem` sem.ltsInitial) (IM.lookup s.val classIdOf) (IM.lookup s.val originOf) (nodeLocOf qd)
            | (qd, s) <- HM.toList sem.ltsStates
            ]
        graph = Graph sem.ltsInitial (getMeta @NoFreeInputsKey lts) nodes (edgesOf tv.tvNorm tm bendsMap sem)
    pure (LtsDocument KEquiv graph (Just (map IS.toList classesIS)) bisim (Just bisimType) (IM.toList posMap) detail (Just (tauTagsOf tv tm sem)))
  where
    sem = lts.val
    tm = buildTagLocs sem
    rootPos = IM.fromList (zip sem.ltsInitial [0 ..])
    originOf = IM.mapMaybe (`IM.lookup` rootPos) sem.ltsRoot

equivDocument :: GVGenOptions -> Equivalence -> Rooting -> TagView -> String -> DecSemanticsFC -> IO (Either BisimError LtsDocument)
equivDocument opts equiv rooting tv bisimType lts = do
    l <- layoutDot (gv opts{onlyDots = True} lts.val)
    pure (equivDocumentWith opts equiv rooting tv bisimType l lts)

minimizeDocumentWith :: GVGenOptions -> Equivalence -> TagView -> Layout -> DecSemanticsFC -> Either BisimError LtsDocument
minimizeDocumentWith opts equiv tv (posMap, bendsMap) lts = do
    mlts <- minimalLTSUnder equiv tv lts
    classesIS <- bisimilarityClassesUnder equiv tv lts
    let nodes =
            [ NodeSummary idx dim br ps (idx `elem` mlts.ltsInitial) (Just idx) Nothing loc
            | (members, s) <- HM.toList mlts.ltsStates
            , let idx = s.val
            , let (dim, br, ps, loc) = case repOf members of
                    Just (qd, _) -> (qdistDim qd, branchesOf qd, procShortOf opts qd, nodeLocOf qd)
                    Nothing -> (0, 0, "nil", Nothing)
            ]
        edges = quotientEdges tv tm sem bendsMap mlts
        graph = Graph mlts.ltsInitial (getMeta @NoFreeInputsKey lts) nodes edges
        detail =
            [ (s.val, detailOf opts qd envOp)
            | (members, s) <- HM.toList mlts.ltsStates
            , Just (qd, envOp) <- [repOf members]
            ]
    pure (LtsDocument KMinimize graph (Just (map IS.toList classesIS)) Nothing Nothing (IM.toList posMap) detail (Just (tauTagsOf tv tm sem)))
  where
    sem = lts.val
    tm = buildTagLocs sem
    origByIdx = IM.fromList [(s.val, (qd, getEnv s)) | (qd, s) <- HM.toList sem.ltsStates]
    repOf members = IM.lookup (minimum members) origByIdx

quotientEdges :: TagView -> Map Integer FC -> Semantics '[LocKV] -> Map (Int, Int) [Pos] -> MinimalSemantics '[LocKV] -> [Edge]
quotientEdges tv tm sem bendsMap mlts =
    [ Edge f t (map (normActionLabel tm) nacts) [(origLabel f t a, render1 (pretty a)) | a <- nacts] (Map.findWithDefault [] (f, t) bendsMap)
    | ((f, t), acts) <- HM.toList mlts.ltsTransitions
    , let nacts = HS.toList acts
    ]
  where
    classOf = IM.fromList [(i, s.val) | (members, s) <- HM.toList mlts.ltsStates, i <- members]
    originals =
        HM.fromListWith
            (++)
            [ ((cf, ct, toNormAction tv.tvNorm a), [render1 (pretty a)])
            | ((u, v), acts) <- HM.toList sem.ltsTransitions
            , Just cf <- [IM.lookup u classOf]
            , Just ct <- [IM.lookup v classOf]
            , a <- HS.toList acts
            ]
    origLabel f t a = case HM.lookup (f, t, a) originals of
        Just os -> intercalate ", " (nub (sort os))
        Nothing -> render1 (pretty a)

equivQuotient :: Equivalence -> TagView -> DecSemanticsFC -> Either BisimError (MinimalSemantics '[LocKV], [Int])
equivQuotient equiv tv lts = do
    classes <- bisimilarityClassesUnder equiv tv lts
    let sideOf s = IM.lookup s originOf
        refined =
            [ (part, ci)
            | (cls, ci) <- zip classes [0 :: Int ..]
            , part <- Map.elems (Map.fromListWith IS.union [(sideOf s, IS.singleton s) | s <- IS.toList cls])
            ]
    pure (quotientLTS equiv tv lts (map fst refined), map snd refined)
  where
    sem = lts.val
    rootPos = IM.fromList (zip sem.ltsInitial [0 :: Int ..])
    originOf = IM.mapMaybe (`IM.lookup` rootPos) sem.ltsRoot

equivMinimizedDocumentWith :: GVGenOptions -> Equivalence -> Rooting -> TagView -> String -> Layout -> DecSemanticsFC -> Either BisimError LtsDocument
equivMinimizedDocumentWith opts equiv rooting tv bisimType (posMap, bendsMap) lts = do
    (mlts, jointOf) <- equivQuotient equiv tv lts
    classesIS <- bisimilarityClassesUnder equiv tv lts
    bisim <- case sem.ltsInitial of
        [a, b] -> Just <$> checkBisimilar equiv rooting tv lts a b
        _ -> Right Nothing
    let jointClass = IM.fromList (zip [0 ..] jointOf)
        rootPos = IM.fromList (zip sem.ltsInitial [0 :: Int ..])
        originOfState s = IM.lookup s sem.ltsRoot >>= (`IM.lookup` rootPos)
        origByIdx = IM.fromList [(s.val, (qd, getEnv s)) | (qd, s) <- HM.toList sem.ltsStates]
        repOf members = IM.lookup (minimum members) origByIdx
        nodes =
            [ NodeSummary idx dim br ps (idx `elem` mlts.ltsInitial) (IM.lookup idx jointClass) (originOfState (minimum members)) loc
            | (members, s) <- HM.toList mlts.ltsStates
            , let idx = s.val
            , let (dim, br, ps, loc) = case repOf members of
                    Just (qd, _) -> (qdistDim qd, branchesOf qd, procShortOf opts qd, nodeLocOf qd)
                    Nothing -> (0, 0, "nil", Nothing)
            ]
        graph = Graph mlts.ltsInitial (getMeta @NoFreeInputsKey lts) nodes (quotientEdges tv tm sem bendsMap mlts)
        detail =
            [ (s.val, detailOf opts qd envOp)
            | (members, s) <- HM.toList mlts.ltsStates
            , Just (qd, envOp) <- [repOf members]
            ]
    pure (LtsDocument KEquiv graph (Just (map IS.toList classesIS)) bisim (Just bisimType) (IM.toList posMap) detail (Just (tauTagsOf tv tm sem)))
  where
    sem = lts.val
    tm = buildTagLocs sem

minimizeDocument :: GVGenOptions -> Equivalence -> TagView -> DecSemanticsFC -> IO (Either BisimError LtsDocument)
minimizeDocument opts equiv tv lts = case minimalLTSUnder equiv tv lts of
    Left e -> pure (Left e)
    Right mlts -> do
        l <- layoutDot (gvMinimal opts{onlyDots = True} mlts)
        pure (minimizeDocumentWith opts equiv tv l lts)

edgesOf :: (Tag -> Tag) -> Map Integer FC -> Map (Int, Int) [Pos] -> Semantics '[LocKV] -> [Edge]
edgesOf norm tm bendsMap sem =
    [ Edge f t (map (actionLabel tm) as) [(render1 (pretty a), render1 (pretty (toNormAction norm a))) | a <- as] (Map.findWithDefault [] (f, t) bendsMap)
    | ((f, t), acts) <- HM.toList sem.ltsTransitions
    , let as = HS.toList acts
    ]

detailMap :: GVGenOptions -> Semantics '[LocKV] -> [(Int, NodeDetail)]
detailMap opts sem =
    [(s.val, detailOf opts qd (getEnv s)) | (qd, s) <- HM.toList sem.ltsStates]

buildStateIndex :: Semantics '[LocKV] -> IntMap (QDist ProcessFC, DensityOp)
buildStateIndex sem =
    IM.fromList [(s.val, (qd, getEnv s)) | (qd, s) <- HM.toList sem.ltsStates]

nodeDetailAt :: GVGenOptions -> IntMap (QDist ProcessFC, DensityOp) -> Int -> Maybe NodeDetail
nodeDetailAt opts idx i = uncurry (detailOf opts) <$> IM.lookup i idx
