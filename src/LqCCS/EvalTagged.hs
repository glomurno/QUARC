{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : LqCCS.EvalTagged
Description : Tagged operational semantics and LTS construction for lqCCS.

Implements the small-step operational semantics of lqCCS, producing a labeled
transition system ('Semantics') whose states are quantum distributions
('QDist') over process configurations and whose transitions are labeled by
'Action' values.

Each action-prefixed process constructor carries a user-supplied integer tag
(see 'LqCCS.Types'). These tags label transitions in the LTS as 'ActionTag'
(single action) or 'SynchTag' (synchronization of two complementary
send\/receive pairs), ensuring that non-deterministic and quantum behaviour is
tracked correctly.

== LTS state metadata

Each state in a 'Semantics' LTS carries an @\"env\"@ field ('EnvKV') storing
the 'DensityOp' of qubits no longer used by any subprocess (the quantum
environment traced out from the full state).

The wrapper 'DecSemantics' adds a 'NoFreeInputsKV' flag that is 'True' when
the LTS contains no free quantum receive transitions; this is required for bisimilarity
to be well-defined.
-}
module LqCCS.EvalTagged (
    Action (..),
    ActionFC,
    DecSemantics,
    DecSemanticsFC,
    EnvBox (..),
    EnvKey,
    EnvKV,
    EvalError (..),
    NoFreeInputsKey,
    NoFreeInputsKV,
    QDistFC,
    Semantics,
    SemanticsFC,
    env,
    eval,
    evalExpr,
    evalPar,
    freeQubits,
    getEnv,
    inUsage,
    isQuantumRecv,
    simplify,
    step,
    subst,
    subStep,
    substExpr,
    usesQubit,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (State, evalState, gets, modify, put)
import Data.Bifunctor (second)
import Data.Bits (xor, (.&.), (.|.))
import Data.Foldable (foldlM)
import Data.HashMap.Internal qualified as HMI
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable (..))
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.LTS
import Data.List (tails)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Traversable (forAccumM)
import Data.Tuple (swap)
import Data.WithData
import GHC.Generics (Generic)
import Numeric.LinearAlgebra.Data (realPart)

import LqCCS.QDist
import LqCCS.Quantum qualified as Q
import LqCCS.Types

-- | Errors that can occur during evaluation.
data EvalError
    = -- | An internal invariant was violated.
      InternalError !FC !String
    | {- | A qubit variable could not be resolved to a concrete qubit index at
      evaluation time.
      -}
      FreeQVar !FC
    deriving (Eq, Show)

data Context meta = Context
    { declarations :: !(DeclMap meta)
    , currentRoot :: !Int
    }

type M meta a = ExceptT EvalError (State (Context meta)) a

-- | Partially evaluate an expression by constant folding.
evalExpr :: Expr meta -> Expr meta
evalExpr = fmap go
  where
    go (IntLit val) = IntLit val
    go (BoolLit val) = BoolLit val
    go (QubitLit val) = QubitLit val
    go (Ref var) = Ref var
    go (Eq_ lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> BoolLit (v1 == v2)
        (WD _ (BoolLit v1), WD _ (BoolLit v2)) -> BoolLit (v1 == v2)
        (WD _ (QubitLit v1), WD _ (QubitLit v2)) -> BoolLit (v1 == v2)
        (lhs', rhs') -> Eq_ lhs' rhs'
    go (LEq lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> BoolLit (v1 <= v2)
        (lhs', rhs') -> LEq lhs' rhs'
    go (And lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (BoolLit False), _) -> BoolLit False
        (_, WD _ (BoolLit False)) -> BoolLit False
        (WD _ (BoolLit True), rhs') -> rhs'.val
        (lhs', WD _ (BoolLit True)) -> lhs'.val
        (lhs', rhs') -> And lhs' rhs'
    go (Or lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (BoolLit True), _) -> BoolLit True
        (_, WD _ (BoolLit True)) -> BoolLit True
        (WD _ (BoolLit False), rhs') -> rhs'.val
        (lhs', WD _ (BoolLit False)) -> lhs'.val
        (lhs', rhs') -> Or lhs' rhs'
    go (Plus lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit 0), rhs') -> rhs'.val
        (lhs', WD _ (IntLit 0)) -> lhs'.val
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> IntLit (v1 + v2)
        (lhs', rhs') -> Plus lhs' rhs'
    go (Sub lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (lhs', WD _ (IntLit 0)) -> lhs'.val
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> IntLit (v1 - v2)
        (lhs', rhs') -> Sub lhs' rhs'
    go (Mul lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit 0), _) -> IntLit 0
        (_, WD _ (IntLit 0)) -> IntLit 0
        (WD _ (IntLit 1), rhs') -> rhs'.val
        (lhs', WD _ (IntLit 1)) -> lhs'.val
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> IntLit (v1 * v2)
        (lhs', rhs') -> Mul lhs' rhs'
    go (Not lhs) = case evalExpr lhs of
        WD _ (BoolLit v) -> BoolLit (not v)
        lhs' -> Not lhs'
    go (Neg lhs) = case evalExpr lhs of
        WD _ (IntLit v) -> IntLit (-v)
        lhs' -> Neg lhs'
    go (BAnd lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> IntLit (v1 .&. v2)
        (lhs', rhs') -> BAnd lhs' rhs'
    go (BOr lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> IntLit (v1 .|. v2)
        (lhs', rhs') -> BOr lhs' rhs'
    go (BXor lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> IntLit (xor v1 v2)
        (lhs', rhs') -> BXor lhs' rhs'
    go (Lt lhs rhs) = case (evalExpr lhs, evalExpr rhs) of
        (WD _ (IntLit v1), WD _ (IntLit v2)) -> BoolLit (v1 < v2)
        (lhs', rhs') -> Lt lhs' rhs'

getIndices :: [WithLoc QExpr] -> M meta [Int]
getIndices = traverse toInt
  where
    toInt (WD _ (QConst i)) = pure $ fromIntegral i
    toInt var = throwError $ FreeQVar (getLoc var)

-- | Capture-avoiding substitution in expressions.
substExpr :: Var -> Expr meta -> Expr meta -> Expr meta
substExpr v e = fmap go
  where
    go (IntLit x) = IntLit x
    go (BoolLit x) = BoolLit x
    go (QubitLit x) = QubitLit x
    go (Ref var) = if v.varname == var.varname then val e else Ref var
    go (Eq_ lhs rhs) = Eq_ (substExpr v e lhs) (substExpr v e rhs)
    go (LEq lhs rhs) = LEq (substExpr v e lhs) (substExpr v e rhs)
    go (And lhs rhs) = And (substExpr v e lhs) (substExpr v e rhs)
    go (Or lhs rhs) = Or (substExpr v e lhs) (substExpr v e rhs)
    go (Plus lhs rhs) = Plus (substExpr v e lhs) (substExpr v e rhs)
    go (Sub lhs rhs) = Sub (substExpr v e lhs) (substExpr v e rhs)
    go (Mul lhs rhs) = Mul (substExpr v e lhs) (substExpr v e rhs)
    go (Not lhs) = Not (substExpr v e lhs)
    go (Neg lhs) = Neg (substExpr v e lhs)
    go (BAnd lhs rhs) = BAnd (substExpr v e lhs) (substExpr v e rhs)
    go (BOr lhs rhs) = BOr (substExpr v e lhs) (substExpr v e rhs)
    go (BXor lhs rhs) = BXor (substExpr v e lhs) (substExpr v e rhs)
    go (Lt lhs rhs) = Lt (substExpr v e lhs) (substExpr v e rhs)

substQVar :: Var -> Expr meta -> WithLoc QExpr -> WithLoc QExpr
substQVar (Var var TQuant) (WD _ (QubitLit i)) (WD meta (QVar var')) | var == var' = WD meta (QConst i)
substQVar _ _ q = q

-- | Capture-avoiding substitution in processes.
subst :: Var -> Expr meta -> Process meta -> Process meta
subst v e = fmap go
  where
    go (Discard args) = Discard (substQVar v e <$> args)
    go (Tau tag cont) = Tau tag (subst v e cont)
    go (Tau2 tag1 tag2 cont) = Tau2 tag1 tag2 (subst v e cont)
    go (Send tag ch val cont) = Send tag ch (substExpr v e val) (subst v e cont)
    go (Recv tag ch var cont) | varname v == varname (val var) = Recv tag ch var cont
    go (Recv tag ch var cont) = Recv tag ch var (subst v e cont)
    go (Apply tag op args cont) = Apply tag op (substQVar v e <$> args) (subst v e cont)
    go (Meas tag meas args var cont) | varname v == varname (val var) = Meas tag meas (substQVar v e <$> args) var cont
    go (Meas tag meas args var cont) = Meas tag meas (substQVar v e <$> args) var (subst v e cont)
    go (ITE guard lhs rhs) = ITE (substExpr v e guard) (subst v e lhs) (subst v e rhs)
    go (Sum ps) = Sum (map (subst v e) ps)
    go (Par ps) = Par (map (subst v e) ps)
    go (Restr proc ch) = Restr (subst v e proc) ch
    go (Call name qargs chargs vargs) = Call name (substQVar v e <$> qargs) chargs (substExpr v e <$> vargs)

-- | An observable action, carrying the tag from the source process.
data Action meta
    = -- | An internal step from a single @tau@ prefix with the given tag.
      ActionTau !Integer
    | {- | A synchronized step produced by a send\/receive pair; carries the
      tag of the send and the tag of the receive.
      -}
      ActionTau2 !Integer !Integer
    | -- | A send of a concrete value on a channel, with the action tag.
      ActionSend !Integer !Channel !(Expr meta)
    | -- | Reception of a concrete value on a channel.
      ActionRecvE !Integer !Channel !(Expr meta)
    deriving (Eq, Generic, Show)

-- | 'Action' with source-location metadata.
type ActionFC = Action '[LocKV]

instance Hashable (Action meta)

-- | Returns 'True' if the action is a qubit receive.
isQuantumRecv :: Action meta -> Bool
isQuantumRecv (ActionRecvE _ (Channel _ TQuant) _) = True
isQuantumRecv _ = False

-- | A quantum distribution over processes with source-location metadata.
type QDistFC = QDist (Process '[LocKV])

maybeQConst :: WithLoc QExpr -> Maybe Integer
maybeQConst (WD _ (QConst i)) = Just i
maybeQConst _ = Nothing

-- | The set of concrete qubit indices referenced anywhere in a process.
inUsage :: Process meta -> HS.HashSet Integer
inUsage (WD _ (Discard args)) = HS.fromList $ mapMaybe maybeQConst args
inUsage (WD _ (Tau _ cont)) = inUsage cont
inUsage (WD _ (Tau2 _ _ cont)) = inUsage cont
inUsage (WD _ (Send _ _ (WD _ (QubitLit i')) cont)) = HS.insert i' $ inUsage cont
inUsage (WD _ (Send _ _ _ cont)) = inUsage cont
inUsage (WD _ (Recv _ _ _ cont)) = inUsage cont
inUsage (WD _ (Apply _ _ args cont)) = HS.union (HS.fromList $ mapMaybe maybeQConst args) (inUsage cont)
inUsage (WD _ (Meas _ _ args _ cont)) = HS.union (HS.fromList $ mapMaybe maybeQConst args) (inUsage cont)
inUsage (WD _ (ITE _ lhs rhs)) = HS.union (inUsage lhs) (inUsage rhs)
inUsage (WD _ (Sum ps)) = HS.unions (map inUsage ps)
inUsage (WD _ (Par ps)) = HS.unions (map inUsage ps)
inUsage (WD _ (Restr proc _)) = inUsage proc
inUsage (WD _ (Call _ qargs _ _)) = HS.fromList $ mapMaybe maybeQConst qargs

-- | Returns 'True' if qubit index @i@ appears in the process.
usesQubit :: Integer -> Process meta -> Bool
usesQubit i p = HS.member i (inUsage p)
{-# INLINE usesQubit #-}

{- | Qubit literals for all qubits in the quantum state that are not currently
referenced by the process.
-}
freeQubits :: QState -> Process meta -> [Expr meta]
freeQubits qs p =
    let used = inUsage p
     in [WD (metadata p) (QubitLit i) | i <- [0 .. qs.dim - 1], not (HS.member i used)]
{-# INLINE freeQubits #-}

allBitsValues :: Int -> WithData meta a -> [Expr meta]
allBitsValues n carrier = [WD (metadata carrier) (IntLit i) | i <- [0 .. 2 ^ n - 1]]

allBoolValues :: WithData meta a -> [Expr meta]
allBoolValues carrier = [WD (metadata carrier) (BoolLit b) | b <- [False, True]]

maskSend :: Types -> Expr meta -> Expr meta
maskSend (TBits n) (WD m (IntLit k)) = WD m (IntLit (k `mod` (2 ^ n)))
maskSend _ x = x

lookupDecl :: WithLoc String -> M meta (Declaration meta)
lookupDecl x = do
    decls <- gets declarations
    case HM.lookup x.val decls of
        Nothing -> throwError $ InternalError (getLoc x) ("Declaration not found at eval time: " ++ x.val)
        Just d -> pure d

substCh :: String -> Channel -> Process meta -> Process meta
substCh paramName actualCh = fmap go
  where
    go (Tau t p) = Tau t (substCh paramName actualCh p)
    go (Tau2 t1 t2 p) = Tau2 t1 t2 (substCh paramName actualCh p)
    go (Send t ch e p) = Send t (renameCh ch) e (substCh paramName actualCh p)
    go (Recv t ch v p) = Recv t (renameCh ch) v (substCh paramName actualCh p)
    go (Apply t op args p) = Apply t op args (substCh paramName actualCh p)
    go (Meas t m args v p) = Meas t m args v (substCh paramName actualCh p)
    go (ITE g lhs rhs) = ITE g (substCh paramName actualCh lhs) (substCh paramName actualCh rhs)
    go (Sum ps) = Sum (map (substCh paramName actualCh) ps)
    go (Par ps) = Par (map (substCh paramName actualCh) ps)
    go (Restr p ch) = Restr (substCh paramName actualCh p) (renameCh ch)
    go (Call n qargs chargs vargs) = Call n qargs (map renameCh chargs) vargs
    go other = other
    renameCh wch
        | chname wch.val == paramName = WD wch.metadata actualCh
        | otherwise = wch

subStepM :: SubState (Process meta) -> M meta [(Action meta, QDist (Process meta))]
subStepM (SubState _ (WD _ (Discard _))) =
    pure []
subStepM (SubState qs (WD _ (Tau t p))) =
    pure [(ActionTau (val t), single qs p)]
subStepM (SubState qs (WD _ (Tau2 t1 t2 p))) =
    pure [(ActionTau2 (val t1) (val t2), single qs p)]
subStepM (SubState qs (WD _ (Send t ch (evalExpr -> v) p))) =
    if isValue v
        then pure [(ActionSend (val t) (val ch) (maskSend (chtype (val ch)) v), single qs p)]
        else pure []
subStepM (SubState qs (WD _ (Recv t ch var p))) =
    case chtype (val ch) of
        TQuant ->
            pure [(ActionRecvE (val t) (val ch) fq, QDist [SubState qs (subst (val var) fq p)]) | fq <- freeQubits qs p]
        TBits n ->
            pure [(ActionRecvE (val t) (val ch) v, QDist [SubState qs (subst (val var) v p)]) | v <- allBitsValues n p]
        TBool ->
            pure [(ActionRecvE (val t) (val ch) v, QDist [SubState qs (subst (val var) v p)]) | v <- allBoolValues p]
        _ -> throwError $ InternalError NoFC "unresolved bits width at receive; add an explicit type annotation"
subStepM (SubState qs (WD _ (Apply t op args p))) = do
    idxs <- getIndices args
    let qs' = qs{rho = Q.applyLocalSop (fromIntegral qs.dim) idxs (val op).opKraus qs.rho}
    pure [(ActionTau (val t), single qs' p)]
subStepM (SubState qs (WD meta (Meas t meas args var p))) = do
    idxs <- getIndices args
    let substates = fmap (\(p', r) -> SubState (QState qs.dim r) p') $ HM.toList $ foldl' (computeMeasure idxs p) HM.empty $ zip [0 ..] (val meas).measKraus.krausOps
    pure [(ActionTau (val t), QDist substates)]
  where
    computeMeasure idxs k hm (i, m) =
        let mrho = DensityOp (Q.applyLocalKraus (fromIntegral qs.dim) idxs [m] qs.rho.unDensityOp)
            prob = abs $ realPart $ Q.trace mrho
            k' = subst (val var) (WD meta (IntLit i)) k
         in if prob < Q.defaultApproxEpsilon then hm else HM.insertWith (\(DensityOp a) (DensityOp b) -> DensityOp (a + b)) k' mrho hm
subStepM (SubState qs (WD _ (ITE guard lhs rhs))) =
    case evalExpr guard of
        WD _ (BoolLit True) -> subStepM (SubState qs lhs)
        WD _ (BoolLit False) -> subStepM (SubState qs rhs)
        _ -> pure []
subStepM (SubState qs (WD _ (Sum ps))) = do
    results <- traverse (subStepM . SubState qs) ps
    pure (concat results)
subStepM (SubState qs (WD meta (Par ps))) = do
    steps <- traverse (subStepM . SubState qs) ps
    let indexed = zip [0 ..] steps
    let usages = inUsage <$> ps
    let indepMoves =
            concat
                [ map (second $ \(QDist d) -> QDist (fmap (fmap (\p -> WD meta (mkPar (p : removeAt i ps)))) d)) $
                    filter (\(act, _) -> filterActionList (removeAt i usages) act) si
                | (i, si) <- indexed
                ]
    let rest_without i j = [p | (k, p) <- zip [0 ..] ps, k /= i, k /= j]
    synchMoves <-
        traverse
            (either throwError pure)
            [ r
            | (i, si) : later <- tails indexed
            , (j, sj) <- later
            , (ai, di) <- si
            , (aj, dj) <- sj
            , Just r <- [synch (rest_without i j) (ai, di) (aj, dj)]
            ]
    pure $ indepMoves ++ synchMoves
  where
    removeAt :: Int -> [a] -> [a]
    removeAt _ [] = []
    removeAt 0 (_ : xs) = xs
    removeAt m (x : xs) = let xs' = removeAt (m - 1) xs in x : xs'

    filterActionList restUsages (ActionRecvE _ (Channel _ TQuant) (WD _ (QubitLit k))) = not $ any (HS.member k) restUsages
    filterActionList _ _ = True

    synch rest (ActionSend t1 lch e, ldist) (ActionRecvE t2 rch e', rdist)
        | lch == rch && e == e' = Just (synch_dists rest t1 t2 ldist rdist)
    synch rest (ActionRecvE t1 lch e, ldist) (ActionSend t2 rch e', rdist)
        | lch == rch && e == e' = Just (synch_dists rest t1 t2 ldist rdist)
    synch _ _ _ = Nothing

    synch_dists rest t1 t2 ldist rdist = case (ldist, rdist) of
        (QDist [SubState _ left], QDist [SubState _ right]) ->
            Right (ActionTau2 (min t1 t2) (max t1 t2), single qs (WD meta (mkPar (left : right : rest))))
        _ -> Left $ InternalError NoFC "Non tau action resulting in a distribution"
subStepM (SubState qs (WD meta (Call name qargs chargs vargs))) = do
    decl <- lookupDecl name
    let subq = foldl' substQArg decl.decBody (zip decl.decQArgs qargs)
    let subch = foldl' substChArg subq (zip decl.decChArgs chargs)
    let subv = foldl' substVArg subch (zip decl.decVArgs vargs)
    subStepM (SubState qs subv)
  where
    qExprArg (QConst i) = QubitLit i
    qExprArg (QVar v) = Ref (Var v TQuant)
    substQArg p (pname, q) = subst (Var pname.val TQuant) (WD meta (qExprArg q.val)) p
    substChArg p ((pname, _), ch) = substCh pname.val (val ch) p
    substVArg p ((pname, ty), e) = subst (Var pname.val ty.val) e p
subStepM (SubState qs (WD meta (Restr p ch))) = do
    steps <- subStepM (SubState qs p)
    let filtered = filter (usesChannel (val ch) . fst) steps
    let addRestr (SubState qs' p') = SubState qs' (WD meta (Restr p' ch))
    let restricted = map (second (\(QDist qd) -> QDist (addRestr <$> qd))) filtered
    pure restricted
  where
    usesChannel :: Channel -> Action meta -> Bool
    usesChannel _ (ActionTau _) = True
    usesChannel _ (ActionTau2 _ _) = True
    usesChannel c (ActionSend _ c' _) = c /= c'
    usesChannel c (ActionRecvE _ c' _) = c /= c'

findActions :: [[(Action meta, QDist (Process meta))]] -> [Action meta]
findActions = HS.toList . HS.fromList . concatMap (fmap fst)

buildDistSteps :: [Action meta] -> [[(Action meta, QDist (Process meta))]] -> [(Action meta, QDist (Process meta))]
buildDistSteps acts dists =
    let dists' = concat [map (act,) $ sequence $ removeEmptyActions $ onlyAction act dists | act <- acts]
        join x y = QState x.dim (DensityOp $ x.rho.unDensityOp + y.rho.unDensityOp)
        merge acc dst = HM.unionWith join acc $ HM.fromListWith join $ map (\(SubState qs p) -> (p, qs)) dst
        merge_all [[ss]] = [ss]
        merge_all dsts = map (uncurry SubState . swap) . HM.toList $ foldl' merge HM.empty dsts
     in map (second (QDist . merge_all)) dists'
  where
    onlyAction :: Action meta -> [[(Action meta, QDist (Process meta))]] -> [[[SubState (Process meta)]]]
    onlyAction act = map (mapMaybe (\(a, QDist qd) -> if a == act then Just qd else Nothing))
    removeEmptyActions = filter (not . null)

stepM :: QDist (Process meta) -> M meta [(Action meta, QDist (Process meta))]
stepM (QDist d) = do
    dists <- traverse subStepM d
    pure $ buildDistSteps (findActions dists) dists

-- | Type-level key for the environment density operator stored per LTS state.
type EnvKey = "env"

{- | A lazily-computed environment density operator. This is a @data@ wrapper
(not a @newtype@) on purpose: it provides a WHNF barrier so that storing it in
the strict metadata record defers the underlying partial-trace computation (see
'env') until a consumer actually forces it via 'getEnv'. Commands that never
inspect the environment (e.g. plain @eval@) thus avoid the cost entirely, while
consumers that do (bisimilarity, pretty-printing) pay it once per
state.
-}
data EnvBox = EnvBox DensityOp

-- | 'WithData' key-value pair associating 'EnvKey' with a lazy 'EnvBox'.
type EnvKV = 'KeyVal EnvKey EnvBox

{- | Extract the environment 'DensityOp' from an LTS state's metadata, forcing
its (lazily deferred) partial-trace computation.
-}
getEnv :: (HasField EnvKey EnvBox fields) => WithData fields a -> DensityOp
getEnv wd = case getMeta @EnvKey wd of EnvBox d -> d
{-# INLINE getEnv #-}

-- | Type-level key for the no-free-inputs flag on a 'DecSemantics'.
type NoFreeInputsKey = "noFreeInputs"

{- | 'WithData' key-value pair associating 'NoFreeInputsKey' with a 'Bool'.
The flag is 'True' when the LTS contains no free quantum receive transitions,
which is required for strong bisimilarity to be well-defined.
-}
type NoFreeInputsKV = 'KeyVal NoFreeInputsKey Bool

{- | The LTS for a single lqCCS process: states are 'QDist' configurations,
transitions are 'Action' sets, and each state carries a 'DensityOp'
environment ('EnvKV').
-}
type Semantics meta = QDistLTS (Process meta) '[EnvKV] HashSet (Action meta)

-- | A 'Semantics' annotated with the 'NoFreeInputsKV' flag.
type DecSemantics meta = WithData '[NoFreeInputsKV] (Semantics meta)

-- | 'Semantics' with source-location metadata.
type SemanticsFC = Semantics '[LocKV]

-- | 'DecSemantics' with source-location metadata.
type DecSemanticsFC = DecSemantics '[LocKV]

type GenLTS meta = WithData '[FreshKV, NoFreeInputsKV] (QDistLTS (Process meta) '[EnvKV] HashSet (Action meta))

emptyGenLTS :: GenLTS meta
emptyGenLTS = WD (Cons 0 (Cons True Nil)) emptyLTS

{- | Compute the environment density operator for a quantum distribution:
the sum of the partial traces of each branch's quantum state over the
qubits currently used by its subprocess. Represents the quantum state of
qubits that have been \"released\" from the process.
-}
env :: QDist (Process meta) -> DensityOp
env (QDist qd) = DensityOp $ sum $ map (\(SubState qs p) -> unDensityOp $ Q.partialTrace (fromIntegral <$> HS.toList (inUsage p)) qs.rho) qd

insertState :: QDist (Process meta) -> Bool -> GenLTS meta -> M meta (Int, Bool, GenLTS meta)
insertState qd isInitial (WD m lts) = do
    let newIdx = select @FreshKey m
        h = HMI.hash qd
        ((idx, fresh), ltsStates') = case HMI.lookup' h qd lts.ltsStates of
            Just (WD _ i) -> ((i, False), lts.ltsStates)
            Nothing ->
                let v = WD (Cons (EnvBox (env qd)) Nil) newIdx
                 in ((newIdx, True), v `seq` HMI.insert' h qd v lts.ltsStates)
        ltsInitial' = if isInitial then lts.ltsInitial ++ [idx] else lts.ltsInitial
    root <- if isInitial then pure newIdx else gets currentRoot
    let ltsRoot' = if fresh then IM.insert newIdx root lts.ltsRoot else lts.ltsRoot
    let lts' = WD m lts{ltsStates = ltsStates', ltsInitial = ltsInitial', ltsRoot = ltsRoot'}
    pure (idx, fresh, if fresh then setMeta @FreshKey (newIdx + 1) lts' else lts')

insertMove :: Index -> Index -> Action meta -> GenLTS meta -> GenLTS meta
insertMove idx idx' act = fmap $ \r ->
    let trans = HM.insertWith HS.union (idx, idx') (HS.singleton act) r.ltsTransitions
        preds = IM.alter (Just . HM.insertWith IS.union act (IS.singleton idx) . fromMaybe HM.empty) idx' r.ltsPredecessors
     in r{ltsTransitions = trans, ltsPredecessors = preds}

eval_ :: QDist (Process meta) -> Index -> GenLTS meta -> M meta (GenLTS meta)
eval_ qd idx lts = do
    next <- stepM qd
    (evaluatedLTS, _) <- forAccumM lts next handleMove
    pure evaluatedLTS
  where
    handleMove acc (act, qd') = do
        (idx', fresh, lts') <- insertState qd' False acc
        let lts'' = insertMove idx idx' act lts'
        lts''' <- if fresh then eval_ qd' idx' lts'' else pure lts''
        pure (updateMeta @NoFreeInputsKey (&& not (isQuantumRecv act)) lts''', ())

evalAddM :: QDist (Process meta) -> GenLTS meta -> M meta (GenLTS meta)
evalAddM qd lts = do
    (idx, fresh, lts') <- insertState qd True lts
    modify $ \r -> r{currentRoot = idx}
    if fresh then eval_ qd idx lts' else pure lts'

evalM :: QDist (Process meta) -> M meta (GenLTS meta)
evalM qd = evalAddM qd emptyGenLTS

{- | Simplify a process before evaluation:

  * Applies 'evalExpr' to all expression subterms.
  * Resolves 'ITE' guards that reduce to a concrete boolean.
  * Removes @'Discard' []@ branches from 'Par', collapsing a single
    remaining branch to itself.
-}
simplify :: Process meta -> Process meta
simplify (WD meta p) = case p of
    Discard args -> WD meta (Discard args)
    Tau tag cont -> WD meta (Tau tag (simplify cont))
    Tau2 tag1 tag2 cont -> WD meta (Tau2 tag1 tag2 (simplify cont))
    Send tag ch e cont -> WD meta (Send tag ch (evalExpr e) (simplify cont))
    Recv tag ch v cont -> WD meta (Recv tag ch v (simplify cont))
    Apply tag op args cont -> WD meta (Apply tag op args (simplify cont))
    Meas tag m args v cont -> WD meta (Meas tag m args v (simplify cont))
    ITE e lhs rhs -> case evalExpr e of
        WD _ (BoolLit True) -> simplify lhs
        WD _ (BoolLit False) -> simplify rhs
        e' -> WD meta (ITE e' (simplify lhs) (simplify rhs))
    Sum ps -> WD meta (mkSum (map simplify ps))
    Par ps ->
        let ps' = filter (\q -> case q.val of Discard [] -> False; _ -> True) (map simplify ps)
         in case ps' of
                [] -> WD meta (Discard [])
                [q] -> q
                _ -> WD meta (mkPar ps')
    Restr proc ch -> WD meta (Restr (simplify proc) ch)
    Call name qargs chargs vargs -> WD meta (Call name qargs chargs (evalExpr <$> vargs))

{- | Compute one step of the operational semantics for a single 'SubState'
(one branch of a quantum distribution). Returns the list of
@(action, successor distribution)@ pairs reachable in one step.
-}
subStep :: DeclMap meta -> SubState (Process meta) -> Either EvalError [(Action meta, QDist (Process meta))]
subStep decls qd = evalState (runExceptT (subStepM qd)) (Context decls (-1))

-- | Compute one step of the operational semantics for a full 'QDist'.
step :: DeclMap meta -> QDist (Process meta) -> Either EvalError [(Action meta, QDist (Process meta))]
step decls qd = evalState (runExceptT (stepM qd)) (Context decls (-1))

-- | Exhaustively build the LTS for a single quantum distribution.
eval :: DeclMap meta -> QDist (Process meta) -> Either EvalError (DecSemantics meta)
eval decls qd = evalState (runExceptT go) (Context decls (-1))
  where
    go = do
        WD m lts <- evalM qd
        pure $ removeMeta @FreshKey @_ @'[NoFreeInputsKV] (WD m lts)

renameCall :: (String -> String) -> Process meta -> Process meta
renameCall f = fmap go
  where
    go (Discard args) = Discard args
    go (Tau t cont) = Tau t (renameCall f cont)
    go (Tau2 t1 t2 cont) = Tau2 t1 t2 (renameCall f cont)
    go (Send t ch v cont) = Send t ch v (renameCall f cont)
    go (Recv t ch var cont) = Recv t ch var (renameCall f cont)
    go (Apply t op args cont) = Apply t op args (renameCall f cont)
    go (Meas t m args var cont) = Meas t m args var (renameCall f cont)
    go (ITE e lhs rhs) = ITE e (renameCall f lhs) (renameCall f rhs)
    go (Sum ps) = Sum (map (renameCall f) ps)
    go (Par ps) = Par (map (renameCall f) ps)
    go (Restr p ch) = Restr (renameCall f p) ch
    go (Call name qargs chargs vargs) = Call (fmap f name) qargs chargs vargs

namespaceDecls :: Int -> (DeclMap meta, QDist (Process meta)) -> (DeclMap meta, QDist (Process meta))
namespaceDecls i (decls, QDist substates) = (decls', QDist (map go substates))
  where
    pfx name = name ++ "$" ++ show i
    decls' = HM.mapKeys pfx $ HM.map (\d -> d{decBody = renameCall pfx d.decBody}) decls
    go (SubState qs p) = SubState qs (renameCall pfx p)

{- | Build a combined LTS from multiple independent quantum
distributions, each paired with its own declaration map. All distributions
are added as initial states of the same LTS. Used when comparing processes
from different sources (e.g. a process and its specification) under
bisimilarity.
-}
evalPar :: [(DeclMap meta, QDist (Process meta))] -> Either EvalError (DecSemantics meta)
evalPar qds = evalState (runExceptT go) (Context mempty (-1))
  where
    namespacedQds = zipWith namespaceDecls [0 ..] qds
    evalSingle lts (decls, qd) = put (Context decls (-1)) >> evalAddM qd lts
    go = fmap resolveOpenWidths . removeMeta @FreshKey @_ @'[NoFreeInputsKV] <$> foldlM evalSingle emptyGenLTS namespacedQds

resolveOpenWidths :: Semantics meta -> Semantics meta
resolveOpenWidths lts
    | HM.null widths = lts
    | otherwise =
        lts
            { ltsTransitions = HM.map (HS.map fixAction) lts.ltsTransitions
            , ltsPredecessors = IM.map (HM.foldlWithKey' (\m a ps -> HM.insertWith IS.union (fixAction a) ps m) HM.empty) lts.ltsPredecessors
            }
  where
    fixedWidths =
        HM.fromListWith
            HS.union
            [(ch.chname, HS.singleton ch.chtype) | acts <- HM.elems lts.ltsTransitions, Just ch <- actionChannel <$> HS.toList acts, isFixedBits ch.chtype]
    widths = HM.mapMaybe (\ts -> case HS.toList ts of [t] -> Just t; _ -> Nothing) fixedWidths
    isFixedBits (TBits _) = True
    isFixedBits _ = False
    fixChannel ch
        | ch.chtype == TBitsAny, Just t <- HM.lookup ch.chname widths = ch{chtype = t}
        | otherwise = ch
    fixAction (ActionSend t ch e) = ActionSend t (fixChannel ch) e
    fixAction (ActionRecvE t ch e) = ActionRecvE t (fixChannel ch) e
    fixAction a = a
    actionChannel (ActionSend _ ch _) = Just ch
    actionChannel (ActionRecvE _ ch _) = Just ch
    actionChannel _ = Nothing
