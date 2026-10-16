{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : LqCCS.Typecheck
Description : Type inference and linearity checking for lqCCS processes.

Implements bidirectional type inference over the lqCCS AST, transforming
parsed ('RawProcessFC') terms into elaborated ('ProcessFC') terms.

The checker enforces:

  * __Type correctness__: expression types are inferred via unification;
    channel and variable types are propagated consistently across the process.
  * __Qubit linearity__: each qubit must be discarded or sent exactly once.
  * __Branching linearity__: branches in parallel must consume distinct qubits;
    both branches of a choice or conditional must consume the same set of qubits.
  * __No recursion__: declarations may not call themselves directly or
    transitively.
-}
module LqCCS.Typecheck (
    Context (..),
    DeclMapFC,
    DeclResult (..),
    TypecheckError (..),
    infer',
    infer,
    inferExpr,
    inferWithDecls,
    inferDeclarations,
    inferWith,
    processRefs,
) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (State, evalState, get, gets, modify, put, runState)
import Data.Foldable (foldMap', for_, traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (isNothing)
import Data.WithData

import LqCCS.Types

-- | Errors that can be reported during type inference.
data TypecheckError
    = {- | Type mismatch: the expression at 'FC' has the first 'Types' but the
      second 'Types' was expected.
      -}
      TypeMismatch !FC !Types !Types
    | {- | The same qubit appears more than once as an argument to a quantum
      operation; the two 'FC' values are the two occurrences.
      -}
      NonUniqueArgument !FC !FC
    | -- | A qubit was used after it had already been consumed, violating linearity.
      NonLinearUsage !FC
    | {- | The two branches of a choice or conditional consume different qubit
      sets; the list of 'FC' values identifies the differing qubit
      occurrences.
      -}
      WrongBranchLinearity !FC !FC ![FC]
    | -- | A quantum operator name was not found.
      UnsupportedOperator !FC !String
    | -- | A quantum measurement name was not found.
      UnsupportedMeasure !FC !String
    | {- | Wrong number of qubit arguments to a quantum operation: the first
      'Integer' is the number provided, the second is the number expected.
      -}
      WrongNumberOfArguments !FC !Integer !Integer
    | -- | A process call references a declaration name that has not been defined.
      DeclarationNotFound !FC !String
    | -- | A declaration calls itself directly or transitively.
      RecursiveDeclaration !FC !String
    | {- | A declaration body uses a constant qubit index, which is not
      allowed (declarations may only refer to qubit parameters).
      -}
      QConstInDeclaration !FC !Integer
    | {- | A declaration body contains a qubit variable that is not bound by
      the declaration's qubit parameter list.
      -}
      FreeVarInDeclaration !FC !String
    | -- | An internal invariant was violated; indicates a bug in the type-checker.
      InternalError !String
    | {- | A @$name+offset@ tag in a declaration body references a base name
      that was not declared in the @with tag_offset(...)@ clause.
      -}
      UnknownOffsetBase !FC !String
    | -- | A call site is missing a required @with name=value@ for a declared @tag_offset@ base.
      MissingOffsetValue !FC !String !String
    | {- | A call site supplies @with name=value@ for a base name that is not
      declared in the declaration's @with tag_offset(...)@ clause.
      -}
      ExtraOffsetValue !FC !String !String
    | -- | A call passes something other than a channel name for a channel parameter.
      ExpectedChannelArgument !FC !String !String
    | -- | A declaration body uses one of its channel parameters as a value (e.g. @if c = 0@).
      ChannelAsValue !FC !String
    deriving (Eq, Show)

-- | The mutable state threaded through type inference.
data Context = Context
    { ops :: !OpsMap
    -- ^ Available quantum operators and measurements.
    , chtypes :: !(HashMap String Types)
    -- ^ Inferred types for channel names encountered so far.
    , vartypes :: !(HashMap String Types)
    -- ^ Inferred types for value variable names encountered so far.
    , sigma :: !(HashSet (WithLoc QExpr))
    -- ^ The linear qubit context.
    , declarations :: !(HashMap String (DeclarationFC, HashSet (WithLoc QExpr)))
    -- ^ Elaborated declarations, paired with the qubit usage set of each body.
    , metavar :: !(HashMap Int (Maybe Types))
    {- ^ Unification variable bindings: 'Nothing' means unresolved, 'Just' @t@
    means the variable has been unified with @t@.
    -}
    , fresh :: !Int
    -- ^ Next available unification variable index.
    , tagOffsetBases :: !(HashSet String)
    -- ^ Valid tag-offset base names for the declaration currently being elaborated.
    , tagOffsetMap :: !(HashMap String [(Integer, Integer)])
    -- ^ Accumulates @base_name → [(placeholder_tag, offset)]@ during declaration elaboration.
    , placeholderCounter :: !Integer
    -- ^ Counter for fresh placeholder tag integers (starts at a large negative value).
    , callSiteCounter :: !Int
    -- ^ Counter for generating unique synthetic declaration names at call sites.
    , declChannels :: !(HashSet String)
    -- ^ Channel parameters of the declaration currently being elaborated.
    }

type M a = ExceptT TypecheckError (State Context) a

fromMaybeError :: TypecheckError -> Maybe a -> M a
fromMaybeError err = maybe (throwError err) pure

inContext :: WithLoc QExpr -> M Bool
inContext qe = gets (HS.member qe . sigma)

insertChannel :: String -> Types -> M ()
insertChannel x ty = modify (\r -> r{chtypes = HM.insert x ty r.chtypes})

insertVar :: String -> Types -> M ()
insertVar x ty = modify (\r -> r{vartypes = HM.insert x ty r.vartypes})

insertQExpr :: WithLoc QExpr -> M ()
insertQExpr qe = modify (\r -> r{sigma = HS.insert qe r.sigma})

lookupVar :: String -> M (Maybe Types)
lookupVar x = gets (HM.lookup x . vartypes)

lookupCh :: String -> M (Maybe Types)
lookupCh x = gets (HM.lookup x . chtypes)

resolveMeta :: Int -> Types -> M ()
resolveMeta x t = modify (\r -> r{metavar = HM.insert x (Just t) r.metavar})

lookupMeta :: Int -> M (Maybe Types)
lookupMeta x =
    gets (HM.lookup x . metavar)
        >>= fromMaybeError (InternalError $ "Metavariable " ++ show x ++ " not found")

lookupDecl :: WithLoc String -> M (DeclarationFC, HashSet (WithLoc QExpr))
lookupDecl x =
    gets (HM.lookup x.val . declarations)
        >>= fromMaybeError (DeclarationNotFound (getLoc x) x.val)

freshMeta :: M Types
freshMeta = do
    ctxt <- get
    put (ctxt{fresh = ctxt.fresh + 1, metavar = HM.insert ctxt.fresh Nothing ctxt.metavar})
    pure $ TVar ctxt.fresh

resolveTag :: WithLoc TagExpr -> M (WithLoc Integer)
resolveTag (WD meta (TConc n)) = pure (WD meta n)
resolveTag wte@(WD meta (TOffset base off)) = do
    valid <- gets (HS.member base . tagOffsetBases)
    unless valid $ throwError (UnknownOffsetBase (getLoc wte) base)
    existing <- gets (HM.lookup base . tagOffsetMap)
    case existing >>= lookup off of
        Just placeholder -> pure (WD meta placeholder)
        Nothing -> do
            placeholder <- gets placeholderCounter
            modify (\s -> s{placeholderCounter = s.placeholderCounter - 1, tagOffsetMap = HM.insertWith (++) base [(placeholder, off)] s.tagOffsetMap})
            pure (WD meta placeholder)

applyTagOffsets :: HashMap Integer Integer -> ProcessFC -> ProcessFC
applyTagOffsets subst (WD meta p) = WD meta $ case p of
    Tau t cont -> Tau (sub t) (go cont)
    Tau2 t1 t2 cont -> Tau2 (sub t1) (sub t2) (go cont)
    Send t ch e cont -> Send (sub t) ch e (go cont)
    Recv t ch var cont -> Recv (sub t) ch var (go cont)
    Apply t op qs cont -> Apply (sub t) op qs (go cont)
    Meas t m qs var cont -> Meas (sub t) m qs var (go cont)
    ITE g l r -> ITE g (go l) (go r)
    Sum ps -> Sum (go <$> ps)
    Par ps -> Par (go <$> ps)
    Restr cont ch -> Restr (go cont) ch
    other -> other
  where
    go = applyTagOffsets subst
    sub (WD m i) = WD m (HM.findWithDefault i i subst)

applyCallModifiers ::
    WithLoc String ->
    [DeclModifier] ->
    [RawCallModifierFC] ->
    M (HashMap Integer Integer)
applyCallModifiers callSite declMods rawCallMods = do
    let declBases = [base | TagOffsetMod base _ <- declMods]
    let callBases = [name | RawTagOffsetValue name _ <- rawCallMods]
    for_ (filter (`notElem` declBases) callBases) $ \b ->
        throwError (ExtraOffsetValue (getLoc callSite) (declDisplayName callSite.val) b)
    for_ (filter (`notElem` callBases) declBases) $ \b ->
        throwError (MissingOffsetValue (getLoc callSite) (declDisplayName callSite.val) b)
    substs <- mapM buildSubst declMods
    pure (HM.unions substs)
  where
    buildSubst (TagOffsetMod base pairs) = do
        rawExpr <-
            fromMaybeError (InternalError "missing call modifier after validation") $
                lookup base [(name, e) | RawTagOffsetValue name e <- rawCallMods]
        (elab, _) <- inferExprM rawExpr
        n <- case elab.val of
            IntLit n -> pure n
            _ -> throwError $ InternalError "tag_offset value must be an integer literal"
        pure $ HM.fromList [(placeholder, n + off) | (placeholder, off) <- pairs]

getDecl :: WithLoc String -> [RawExprFC] -> [RawExprFC] -> [RawCallModifierFC] -> M ProcessFC
getDecl name rawqargs rawcvargs rawcallmods = do
    (decl, usageSet) <- lookupDecl name
    checkOpSize rawqargs (fromIntegral $ length decl.decQArgs)
    checkOpSize rawcvargs (fromIntegral $ length decl.decChArgs + length decl.decVArgs)
    let (rawchargs, rawvargs) = splitAt (length decl.decChArgs) rawcvargs
    qargs <- traverse (inferQExpr True) rawqargs
    let qparams = HM.fromList $ zip (val <$> decl.decQArgs) (val <$> qargs)
    qusage <- HS.fromList <$> traverse (substUsage qparams) (HS.toList usageSet)
    modify (\c -> c{sigma = HS.union c.sigma qusage})
    chargs <- traverse checkChannels (zip rawchargs decl.decChArgs)
    vargs <- traverse checkArgs (zip rawvargs decl.decVArgs)
    callTarget <-
        if null decl.decModifiers && null rawcallmods
            then pure name
            else do
                subst <- applyCallModifiers name decl.decModifiers rawcallmods
                let synthBody = applyTagOffsets subst decl.decBody
                let synthDecl = Declaration decl.decQArgs decl.decChArgs decl.decVArgs [] synthBody
                counter <- gets callSiteCounter
                modify (\s -> s{callSiteCounter = s.callSiteCounter + 1})
                let synthName = name.val ++ "__synth" ++ show counter
                modify (\s -> s{declarations = HM.insert synthName (synthDecl, usageSet) s.declarations})
                pure (WD name.metadata synthName)
    pure $ WD name.metadata (Call callTarget qargs chargs vargs)
  where
    checkChannels (rawCh, (paramName, paramTy)) =
        case rawCh.val of
            RVar chName -> do
                chty <- inferChannel (WD rawCh.metadata chName) (Just paramTy)
                forcedChTy <- force chty
                pure $ WD rawCh.metadata (Channel chName forcedChTy)
            _ -> throwError $ ExpectedChannelArgument (getLoc rawCh) (declDisplayName name.val) paramName.val
    checkArgs (rawArg, (_, paramTy)) = do
        (e, ty) <- inferExprM rawArg
        unify (getLoc e) ty paramTy.val
        pure e
    substUsage params q@(WD m (QVar v)) = pure $ maybe q (WD m) (HM.lookup v params)
    substUsage _ _ = throwError $ InternalError ("Unexpected free variable in typechecked declaration " ++ show name.val)

getOp :: WithLoc String -> M Operator
getOp opname = do
    ops <- gets (opSOP . ops)
    fromMaybeError (UnsupportedOperator (getLoc opname) opname.val) $ HM.lookup opname.val ops

getMeas :: WithLoc String -> M Measure
getMeas opname = do
    ops <- gets (opMeas . ops)
    fromMaybeError (UnsupportedMeasure (getLoc opname) opname.val) $ HM.lookup opname.val ops

initialContext :: OpsMap -> Context
initialContext ops = Context ops HM.empty HM.empty HS.empty HM.empty HM.empty 0 HS.empty HM.empty (-1_000_000_000) 0 HS.empty

unifyVar :: FC -> Int -> Types -> M ()
unifyVar _ i (TVar j) | i == j = pure ()
unifyVar loc i t =
    lookupMeta i >>= \case
        Nothing -> resolveMeta i t
        Just TBitsAny ->
            force t >>= \case
                fixed@(TBits _) -> resolveMeta i fixed
                _ -> unify loc TBitsAny t
        Just t' -> unify loc t' t

unify :: FC -> Types -> Types -> M ()
unify loc (TVar i) t = unifyVar loc i t
unify loc t (TVar i) = unifyVar loc i t
unify _ TBitsAny (TBits _) = pure ()
unify _ (TBits _) TBitsAny = pure ()
unify _ TBitsAny TBitsAny = pure ()
unify loc TBitsAny t = throwError $ TypeMismatch loc TBitsAny t
unify loc t TBitsAny = throwError $ TypeMismatch loc t TBitsAny
unify loc (TChan t1) (TChan t2) = unify loc t1 t2
unify loc t t' = unless (t == t') (throwError $ TypeMismatch loc t t')

checkUniqueness :: [WithLoc QExpr] -> M ()
checkUniqueness = go HS.empty
  where
    go _ [] = pure ()
    go hs (qe : xs) =
        maybe
            (go (HS.insert qe hs) xs)
            (\v -> throwError $ NonUniqueArgument (getLoc v) (getLoc qe))
            (HS.lookupElement qe hs)

addUsage :: WithLoc QExpr -> M ()
addUsage qe = do
    usage <- inContext qe
    when usage $ throwError (NonLinearUsage (getLoc qe))
    insertQExpr qe

class HasMeta a where
    force :: a -> M a

instance (HasMeta a, HasMeta b) => HasMeta (a, b) where
    force (a, b) = (,) <$> force a <*> force b

instance (HasMeta a) => HasMeta (WithData meta a) where
    force (WD meta a) = WD meta <$> force a

instance HasMeta Types where
    force (TVar i) = maybe (pure (TVar i)) force =<< lookupMeta i
    force t = pure t

checkLinearity :: [WithLoc QExpr] -> M ()
checkLinearity = traverse_ $ \qe -> do
    isIn <- inContext qe
    unless isIn (throwError $ NonLinearUsage (getLoc qe))

shadowingChannel :: String -> M a -> M a
shadowingChannel x m = do
    outer <- gets declChannels
    modify (\s -> s{declChannels = HS.delete x outer})
    r <- m
    modify (\s -> s{declChannels = outer})
    pure r

inferExprM :: RawExprFC -> M (ExprFC, Types)
inferExprM (WD meta raw) = case raw of
    RIntLit n -> pure (WD meta (IntLit n), TBitsAny)
    RBoolLit b -> pure (WD meta (BoolLit b), TBool)
    RQubitLit qb -> pure (WD meta (QubitLit qb), TQuant)
    RVar x -> do
        mt <- lookupVar x
        isChannelParam <- gets (HS.member x . declChannels)
        when (isNothing mt && isChannelParam) $ throwError $ ChannelAsValue (select @LocKey meta) x
        t <- maybe freshMeta pure mt
        insertVar x t
        pure (WD meta (Ref (Var x t)), t)
    REq e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (select @LocKey meta) t1 t2
        pure (WD meta (Eq_ v1 v2), TBool)
    RLEq e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (LEq v1 v2), TBool)
    RAnd e1 e2 -> do
        (v1, t1) <- inferExprM e1
        unify (getLoc v1) t1 TBool
        (v2, t2) <- inferExprM e2
        unify (getLoc v2) t2 TBool
        pure (WD meta (And v1 v2), TBool)
    ROr e1 e2 -> do
        (v1, t1) <- inferExprM e1
        unify (getLoc v1) t1 TBool
        (v2, t2) <- inferExprM e2
        unify (getLoc v2) t2 TBool
        pure (WD meta (Or v1 v2), TBool)
    RNot e1 -> do
        (v1, t1) <- inferExprM e1
        unify (getLoc v1) t1 TBool
        pure (WD meta (Not v1), TBool)
    RPlus e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (Plus v1 v2), t1)
    RSub e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (Sub v1 v2), t1)
    RMul e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (Mul v1 v2), t1)
    RNeg e1 -> do
        (v1, t1) <- inferExprM e1
        unify (getLoc v1) t1 TBitsAny
        pure (WD meta (Neg v1), t1)
    RBAnd e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (BAnd v1 v2), t1)
    RBOr e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (BOr v1 v2), t1)
    RBXor e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (BXor v1 v2), t1)
    RLt e1 e2 -> do
        (v1, t1) <- inferExprM e1
        (v2, t2) <- inferExprM e2
        unify (getLoc v1) t1 TBitsAny
        unify (getLoc v1) t1 t2
        pure (WD meta (Lt v1 v2), TBool)

instance HasMeta (Expr' meta) where
    force (Ref (Var name ty)) = Ref . Var name <$> force ty
    force (Eq_ e1 e2) = Eq_ <$> force e1 <*> force e2
    force (LEq e1 e2) = LEq <$> force e1 <*> force e2
    force (And e1 e2) = And <$> force e1 <*> force e2
    force (Or e1 e2) = Or <$> force e1 <*> force e2
    force (Not e1) = Not <$> force e1
    force (Plus e1 e2) = Plus <$> force e1 <*> force e2
    force (Sub e1 e2) = Sub <$> force e1 <*> force e2
    force (Mul e1 e2) = Mul <$> force e1 <*> force e2
    force (Neg e1) = Neg <$> force e1
    force (BAnd e1 e2) = BAnd <$> force e1 <*> force e2
    force (BOr e1 e2) = BOr <$> force e1 <*> force e2
    force (BXor e1 e2) = BXor <$> force e1 <*> force e2
    force (Lt e1 e2) = Lt <$> force e1 <*> force e2
    force t = pure t

checkQExpr :: Bool -> ExprFC -> M (WithLoc QExpr)
checkQExpr _ (WD meta (QubitLit qb)) = pure $ WD meta (QConst qb)
checkQExpr ins (WD meta (Ref (Var x t))) =
    lookupVar x >>= \case
        Nothing -> do
            unless ins (throwError $ NonLinearUsage (select @LocKey meta))
            insertVar x t
            pure $ WD meta (QVar x)
        Just _ -> pure $ WD meta (QVar x)
checkQExpr _ _ = throwError $ InternalError "Unexpected non TQuant case when inferring QExpr"

inferQExpr :: Bool -> RawExprFC -> M (WithLoc QExpr)
inferQExpr ins e = do
    (v, t) <- inferExprM e
    unify (getLoc v) t TQuant
    checkQExpr ins v

checkEmptyContext :: FC -> M ()
checkEmptyContext loc = do
    sgm <- gets sigma
    unless (HS.null sgm) $ throwError (NonLinearUsage loc)

checkOpSize :: [RawExprFC] -> Integer -> M ()
checkOpSize exprs size = do
    let len = fromIntegral (length exprs)
    unless (len == size) $ do
        let loc = foldMap' getLoc exprs
        throwError $ WrongNumberOfArguments loc len size

mustHaveSameContext :: M (WithLoc a) -> M (WithLoc b) -> M (WithLoc a, WithLoc b)
mustHaveSameContext k1 k2 = do
    startingCtxt <- gets sigma
    r1 <- k1
    endingCtxt1 <- gets sigma
    modify (\r -> r{sigma = startingCtxt})
    r2 <- k2
    endingCtxt2 <- gets sigma
    unless (endingCtxt1 == endingCtxt2) $ do
        let diff = HS.difference (HS.union endingCtxt1 endingCtxt2) (HS.intersection endingCtxt1 endingCtxt2)
        throwError $ WrongBranchLinearity (getLoc r1) (getLoc r2) (getLoc <$> HS.toList diff)
    pure (r1, r2)

mustHaveDisjointContext :: M (WithLoc a) -> M (WithLoc b) -> M (WithLoc a, WithLoc b)
mustHaveDisjointContext k1 k2 = do
    startingCtxt <- gets sigma
    r1 <- k1
    endingCtxt1 <- gets sigma
    modify (\r -> r{sigma = startingCtxt})
    r2 <- k2
    endingCtxt2 <- gets sigma
    let inter = HS.intersection endingCtxt1 endingCtxt2
    unless (HS.null inter) $ throwError (WrongBranchLinearity (getLoc r1) (getLoc r2) (getLoc <$> HS.toList inter))
    modify (\r -> r{sigma = HS.union endingCtxt1 endingCtxt2})
    pure (r1, r2)

inferChannel :: WithLoc String -> Maybe (WithLoc Types) -> M Types
inferChannel ch mty =
    lookupCh (val ch) >>= \case
        Nothing -> do
            t <- maybe freshMeta (pure . val) mty
            insertChannel (val ch) t
            pure t
        Just t -> case mty of
            Just ty -> unify (getLoc ty) t (val ty) >> pure t
            Nothing -> pure t

inferVar :: WithLoc String -> Maybe (WithLoc Types) -> M Types
inferVar var mty =
    lookupVar (val var) >>= \case
        Nothing -> do
            t <- maybe freshMeta (pure . val) mty
            insertVar (val var) t
            pure t
        Just t -> case mty of
            Just ty -> unify (getLoc ty) t (val ty) >> pure t
            Nothing -> pure t

inferProcessM :: RawProcessFC -> M ProcessFC
inferProcessM (WD meta rawproc) = case rawproc of
    RDiscard rawqubits -> do
        checkEmptyContext (select @LocKey meta)
        qubits <- traverse (inferQExpr True) rawqubits
        checkUniqueness qubits
        traverse_ addUsage qubits
        pure $ WD meta (Discard qubits)
    RTau wtag rawcont -> do
        tag <- resolveTag wtag
        cont <- inferProcessM rawcont
        pure $ WD meta (Tau tag cont)
    RTau2 wtag1 wtag2 rawcont -> do
        tag1 <- resolveTag wtag1
        tag2 <- resolveTag wtag2
        cont <- inferProcessM rawcont
        pure $ WD meta (Tau2 tag1 tag2 cont)
    RSend wtag chname optty rawexpr rawcont -> do
        tag <- resolveTag wtag
        cont <- inferProcessM rawcont
        (expr, exprty) <- inferExprM rawexpr
        chty <- inferChannel chname optty
        unify (getLoc chname) chty exprty
        forcedChty <- force chty
        when (forcedChty == TQuant) $ do
            qubit <- checkQExpr True expr
            addUsage qubit
        let ch = WD (metadata chname) (Channel (val chname) forcedChty)
        pure $ WD meta (Send tag ch expr cont)
    RRecv wtag chname optty varname rawcont -> do
        tag <- resolveTag wtag
        cont <- shadowingChannel varname.val (inferProcessM rawcont)
        chty <- inferChannel chname optty
        varty <- inferVar varname optty
        unify (getLoc varname) chty varty
        forcedVarty <- force varty
        when (forcedVarty == TQuant) $ do
            modify (\c -> c{sigma = HS.delete (WD varname.metadata (QVar varname.val)) c.sigma})
        let ch = WD (metadata chname) (Channel (val chname) chty)
        let var = WD (metadata varname) (Var (val varname) varty)
        pure $ WD meta (Recv tag ch var cont)
    RApply wtag opname rawargs rawcont -> do
        tag <- resolveTag wtag
        cont <- inferProcessM rawcont
        operator <- getOp opname
        checkOpSize rawargs (opDim operator)
        qubits <- traverse (inferQExpr False) rawargs
        checkUniqueness qubits
        checkLinearity qubits
        let op = WD (metadata opname) operator
        pure $ WD meta (Apply tag op qubits cont)
    RMeas wtag measname rawargs varname rawcont -> do
        tag <- resolveTag wtag
        cont <- shadowingChannel varname.val (inferProcessM rawcont)
        measure <- getMeas measname
        checkOpSize rawargs (measSize measure)
        qubits <- traverse (inferQExpr False) rawargs
        checkUniqueness qubits
        checkLinearity qubits
        let nOuts = length (krausOps measure.measKraus)
            resultBits = length $ takeWhile (< nOuts) $ iterate (* 2) 1
        varty <- inferVar varname (Just (WD (metadata varname) (TBits resultBits)))
        let meas = WD (metadata measname) measure
        let var = WD (metadata varname) (Var (val varname) varty)
        pure $ WD meta (Meas tag meas qubits var cont)
    RITE rawexpr rawcontt rawcontf -> do
        (expr, exprty) <- inferExprM rawexpr
        unify (getLoc rawexpr) exprty TBool
        (contt, contf) <- mustHaveSameContext (inferProcessM rawcontt) (inferProcessM rawcontf)
        pure $ WD meta (ITE expr contt contf)
    RSum rawcontl rawcontr -> do
        (contl, contr) <- mustHaveSameContext (inferProcessM rawcontl) (inferProcessM rawcontr)
        pure $ WD meta (mkSum [contl, contr])
    RPar rawcontl rawcontr -> do
        (contl, contr) <- mustHaveDisjointContext (inferProcessM rawcontl) (inferProcessM rawcontr)
        pure $ WD meta (mkPar [contl, contr])
    RRestr rawcont chname -> do
        cont <- inferProcessM rawcont
        chty <- inferChannel chname Nothing
        let ch = WD (metadata chname) (Channel (val chname) chty)
        pure $ WD meta (Restr cont ch)
    RRef name rawqargs rawvargs rawcallmods -> getDecl (WD meta name) rawqargs rawvargs rawcallmods

instance HasMeta Channel where
    force (Channel name ty) = Channel name <$> force ty

instance HasMeta Var where
    force (Var name ty) = Var name <$> force ty

instance HasMeta (Process' meta) where
    force (Tau tag cont) = Tau tag <$> force cont
    force (Tau2 tag1 tag2 cont) = Tau2 tag1 tag2 <$> force cont
    force (Send tag ch expr cont) = Send tag <$> force ch <*> force expr <*> force cont
    force (Recv tag ch var cont) = Recv tag <$> force ch <*> force var <*> force cont
    force (Apply tag op qubits cont) = Apply tag op qubits <$> force cont
    force (Meas tag meas qubits var cont) = Meas tag meas qubits <$> force var <*> force cont
    force (ITE expr contt contf) = ITE <$> force expr <*> force contt <*> force contf
    force (Sum ps) = Sum <$> traverse force ps
    force (Par ps) = Par <$> traverse force ps
    force (Restr cont ch) = Restr <$> force cont <*> force ch
    force (Call name qargs chargs vargs) = Call name qargs <$> traverse force chargs <*> traverse force vargs
    force t = pure t

instance HasMeta (Declaration meta) where
    force (Declaration q ch v mods b) = Declaration q ch v mods <$> force b

addDeclaration :: RawDeclarationFC -> M ()
addDeclaration (WD _ (RawDecl name qparams chparams vparams rawmods rawproc)) = do
    let declaredBases = HS.fromList [val bname | RawTagOffset bname <- rawmods]
    modify (\s -> s{sigma = HS.empty, vartypes = HM.empty, tagOffsetBases = declaredBases, tagOffsetMap = HM.empty, declChannels = HS.fromList (val . fst <$> chparams)})
    let qvars = HS.fromList (val <$> qparams)
    traverse_ (`insertVar` TQuant) qvars
    traverse_ (\(n, t) -> insertChannel (val n) (val t)) chparams
    traverse_ (\(n, t) -> insertVar (val n) (val t)) vparams
    proc <- inferProcessM rawproc >>= force
    sgm <- gets sigma
    for_ sgm $ \case
        WD meta (QConst e) -> throwError $ QConstInDeclaration (select @LocKey meta) e
        WD meta (QVar v) -> unless (HS.member v qvars) $ throwError $ FreeVarInDeclaration (select @LocKey meta) v
    offsetMap <- gets tagOffsetMap
    let modifiers = [TagOffsetMod base pairs | (base, pairs) <- HM.toList offsetMap]
    let decl = Declaration qparams chparams vparams modifiers proc
    modify (\s -> s{declarations = HM.insert (val name) (decl, sgm) s.declarations, declChannels = HS.empty})

refsIn :: RawProcessFC -> [WithLoc String]
refsIn raw = case raw.val of
    RDiscard _ -> []
    RTau _ cont -> refsIn cont
    RTau2 _ _ cont -> refsIn cont
    RSend _ _ _ _ cont -> refsIn cont
    RRecv _ _ _ _ cont -> refsIn cont
    RApply _ _ _ cont -> refsIn cont
    RMeas _ _ _ _ cont -> refsIn cont
    RITE _ lhs rhs -> refsIn lhs ++ refsIn rhs
    RSum lhs rhs -> refsIn lhs ++ refsIn rhs
    RPar lhs rhs -> refsIn lhs ++ refsIn rhs
    RRestr cont _ -> refsIn cont
    RRef name _ _ _ -> [WD (Cons (getLoc raw) Nil) name]

callGraph :: [RawDeclarationFC] -> HashMap String [WithLoc String]
callGraph rawdecls = HM.fromList [(val name, refsIn body) | WD _ (RawDecl name _ _ _ _ body) <- rawdecls]

checkNoRecursion :: [RawDeclarationFC] -> M ()
checkNoRecursion rawdecls = do
    let cg = callGraph rawdecls
    traverse_ (\(WD meta (RawDecl name _ _ _ _ _)) -> go cg (select @LocKey meta) name.val HS.empty) rawdecls
  where
    go cg loc curr visited
        | HS.member curr visited = throwError $ RecursiveDeclaration loc curr
        | otherwise = do
            let visited' = HS.insert curr visited
            let refs = HM.findWithDefault [] curr cg
            traverse_ (\d -> go cg (getLoc d) d.val visited') refs

inferM :: [RawDeclarationFC] -> RawProcessFC -> M ProcessFC
inferM rawdecls rawproc = do
    checkNoRecursion rawdecls
    traverse_ addDeclaration rawdecls
    modify (\s -> s{sigma = HS.empty, vartypes = HM.empty})
    inferProcessM rawproc

{- | Typecheck a process together with its declarations, returning the
elaborated process and the fully resolved 'DeclMapFC' on success.
-}
inferWithDecls :: OpsMap -> [RawDeclarationFC] -> RawProcessFC -> Either TypecheckError (ProcessFC, DeclMapFC)
inferWithDecls ops rawdecls rawproc = evalState (runExceptT go) (initialContext ops)
  where
    go = do
        p <- inferM rawdecls rawproc >>= force
        decls <- gets declarations
        declsF <- traverse (force . fst) decls
        pure (p, declsF)

{- | Typecheck a single expression, returning the elaborated 'ExprFC' and its
inferred 'Types' on success.
-}
inferExpr :: OpsMap -> RawExprFC -> Either TypecheckError (ExprFC, Types)
inferExpr ops rawexpr = evalState (runExceptT (inferExprM rawexpr >>= force)) (initialContext ops)

{- | Typecheck a process (with optional declarations), returning the elaborated
'ProcessFC' on success. Discards the final 'Context'; use 'infer'' to
retain it.
-}
infer :: OpsMap -> [RawDeclarationFC] -> RawProcessFC -> Either TypecheckError ProcessFC
infer ops rawdecls rawproc = evalState (runExceptT (inferM rawdecls rawproc >>= force)) (initialContext ops)

{- | Like 'infer', but also returns the final 'Context', which contains the
inferred channel and variable types. Useful for inspecting inferred types
after a successful check.
-}
infer' :: OpsMap -> [RawDeclarationFC] -> RawProcessFC -> (Either TypecheckError ProcessFC, Context)
infer' ops rawdecls rawproc = runState (runExceptT forcedContext) (initialContext ops)
  where
    forcedContext = do
        p <- inferM rawdecls rawproc >>= force
        ctxt <- get
        chtypesF <- traverse force ctxt.chtypes
        vartypesF <- traverse force ctxt.vartypes
        declsF <- traverse (\(d, s) -> (,s) <$> force d) ctxt.declarations
        put ctxt{chtypes = chtypesF, vartypes = vartypesF, declarations = declsF}
        pure p

-- | Outcome of typechecking a single declaration with 'inferDeclarations'.
data DeclResult
    = -- | The declaration typechecks; carries its elaborated form and qubit usage set.
      DeclOk !DeclarationFC !(HashSet (WithLoc QExpr))
    | -- | The declaration itself fails to typecheck.
      DeclFailed !TypecheckError
    | -- | The declaration was not checked because it calls these failed (or skipped) declarations.
      DeclSkipped ![String]

-- | Typecheck each declaration independently, in order.
inferDeclarations :: OpsMap -> [RawDeclarationFC] -> Either TypecheckError (Context, [(String, DeclResult)])
inferDeclarations ops rawdecls = do
    evalState (runExceptT (checkNoRecursion rawdecls)) (initialContext ops)
    pure $ go (initialContext ops) HS.empty rawdecls
  where
    cg = callGraph rawdecls
    go ctxt _ [] = (ctxt, [])
    go ctxt bad (d@(WD _ (RawDecl name _ _ _ _ _)) : ds) =
        let n = val name
            blockers = [r | r <- val <$> HM.findWithDefault [] n cg, HS.member r bad]
         in if not (null blockers)
                then next ctxt (HS.insert n bad) (n, DeclSkipped (HS.toList (HS.fromList blockers)))
                else case runState (runExceptT (checkOne n d)) ctxt of
                    (Left err, _) -> next ctxt (HS.insert n bad) (n, DeclFailed err)
                    (Right (decl, sgm), ctxt') -> next ctxt' bad (n, DeclOk decl sgm)
      where
        next ctxt' bad' r = let (c, rs) = go ctxt' bad' ds in (c, r : rs)
    checkOne n d = do
        addDeclaration d
        (decl, sgm) <- fromMaybeError (InternalError ("declaration " ++ n ++ " missing after elaboration")) . HM.lookup n =<< gets declarations
        (,sgm) <$> force decl

-- | Typecheck a process against a context produced by 'inferDeclarations'
inferWith :: Context -> RawProcessFC -> (Either TypecheckError ProcessFC, Context)
inferWith ctxt0 rawproc = runState (runExceptT go) ctxt0{sigma = HS.empty, vartypes = HM.empty}
  where
    go = do
        p <- inferProcessM rawproc >>= force
        ctxt <- get
        chtypesF <- traverse force ctxt.chtypes
        vartypesF <- traverse force ctxt.vartypes
        put ctxt{chtypes = chtypesF, vartypes = vartypesF}
        pure p

-- | Names of the declarations called (directly) by a process.
processRefs :: RawProcessFC -> [String]
processRefs = fmap val . refsIn
