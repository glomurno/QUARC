{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : LqCCS.Types
Description : Core AST types and quantum primitives for lqCCS.

Defines the full type hierarchy for the lqCCS process algebra, covering two
distinct phases of the pipeline:

  * __Parsing phase__: @Raw*@ types ('RawExpr'', 'RawProcess'',
    'RawDeclaration'') use string-based names for channels, operators, and
    measures and carry arbitrary metadata (typically source locations via
    'WithLoc').

  * __Elaboration phase__: 'Expr'', 'Process'', 'Declaration' replace
    raw string references with fully resolved 'Channel', 'Var', 'Operator',
    and 'Measure' values after type-checking.

Action-prefixed process constructors (e.g. 'Tau', 'Send', 'Recv') carry a
user-supplied integer __tag__ ('WithLoc' 'Integer') that uniquely identifies
each action occurrence in the source. Tags are required for correct LTS
construction under non-deterministic and quantum behaviour, and are combined
into 'Tag' values ('ActionTag', 'SynchTag') when labeling LTS transitions.
-}
module LqCCS.Types where

import GHC.Generics (Generic)

import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (..))
import Data.List (sortOn)
import Data.Vector.Storable qualified as VS
import Data.WithData
import Numeric.LinearAlgebra (C, Matrix, cols, flatten)
import Text.Printf (printf)

{- | A source span, recording the file name and start\/end positions both 0-indexed.
 - 'NoFC' is used when no source location is available (e.g., for synthesised nodes).
-}
data FC
    = FC !String !(Int, Int) !(Int, Int)
    | NoFC
    deriving (Eq, Generic, Show)

instance Hashable FC

instance Semigroup FC where
    FC f1 s1 e1 <> FC f2 s2 e2
        | f1 == f2 = FC f1 (min s1 s2) (max e1 e2)
        | otherwise = NoFC
    NoFC <> fc = fc
    fc <> NoFC = fc

instance Monoid FC where
    mempty = NoFC

-- | Type-level key for the source-location metadata field.
type LocKey = "loc"

-- | 'WithData' key-value pair associating 'LocKey' with an 'FC'.
type LocKV = 'KeyVal LocKey FC

-- | A value annotated with a single source-location field.
type WithLoc a = WithData '[LocKV] a

-- | Extract the source location from any annotated 'WithData' value.
getLoc :: (HasField LocKey FC fields) => WithData fields a -> FC
getLoc = getMeta @LocKey

-- | The lqCCS type system.
data Types
    = -- | Fixed-width bits type: @bit@ (1-bit) or @bits n@ (n-bit). Values are in @[0, 2^n − 1]@.
      TBits !Int
    | {- | Bits type with unknown width. Used as the default type for integer literals during
      type inference; unifies with any 'TBits' @n@.
      -}
      TBitsAny
    | -- | Boolean type.
      TBool
    | -- | Qubit type.
      TQuant
    | -- | Channel type: @chan T@ transmits values of type @T@.
      TChan !Types
    | -- | Unification variable (used internally during type inference).
      TVar Int
    deriving (Eq, Generic, Show)

instance Hashable Types

-- | A parsed expression carrying metadata @meta@ (typically source locations).
type RawExpr meta = WithData meta (RawExpr' meta)

-- | 'RawExpr' with a source-location field.
type RawExprFC = RawExpr '[LocKV]

{- | The functor of parsed expression nodes. Parameterised by @meta@ so that
sub-expressions carry the same metadata type.
-}
data RawExpr' meta
    = -- | Integer literal: ..., @-1@, @0@, @1@, ...
      RIntLit !Integer
    | -- | Boolean literal: @true@ or @false@
      RBoolLit !Bool
    | -- | Qubit literal: @qb0@, @qb1@, ...
      RQubitLit !Integer
    | -- | Variable reference: @x@, @y@, @z@, ...
      RVar !String
    | -- | Equality test: @e1 = e2@
      REq (RawExpr meta) (RawExpr meta)
    | -- | Less-than-or-equal: @e1 <= e2@
      RLEq (RawExpr meta) (RawExpr meta)
    | -- | Logical conjunction: @e1 /\\ e2@
      RAnd (RawExpr meta) (RawExpr meta)
    | -- | Logical disjunction: @e1 \\/ e2@
      ROr (RawExpr meta) (RawExpr meta)
    | -- | Logical negation: @!e@
      RNot (RawExpr meta)
    | -- | Integer addition: @e1 + e2@
      RPlus (RawExpr meta) (RawExpr meta)
    | -- | Integer subtraction: @e1 - e2@
      RSub (RawExpr meta) (RawExpr meta)
    | -- | Integer multiplication: @e1 * e2@
      RMul (RawExpr meta) (RawExpr meta)
    | -- | Integer negation: @-e@
      RNeg (RawExpr meta)
    | -- | Bitwise AND: @e1 & e2@
      RBAnd (RawExpr meta) (RawExpr meta)
    | -- | Bitwise OR: @e1 | e2@
      RBOr (RawExpr meta) (RawExpr meta)
    | -- | Bitwise XOR: @e1 ^ e2@
      RBXor (RawExpr meta) (RawExpr meta)
    | -- | Integer less-than: @e1 < e2@
      RLt (RawExpr meta) (RawExpr meta)
    deriving (Eq, Generic, Show)

instance Hashable (RawExpr' meta)

-- | LTS transition label produced by the operational semantics.
data Tag
    = ActionTag !Integer
    | SynchTag !Integer !Integer
    deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Tag

{- | Which @tau@ tags are silent under branching bisimilarity.
Combining declarations with '<>' concatenates both lists, and 'mempty'
makes every @tau@ tag silent.
-}
data Silence = Silence
    { silenceSilent :: ![Tag]
    , silenceVisible :: ![Tag]
    }
    deriving (Eq, Show)

instance Semigroup Silence where
    Silence s1 v1 <> Silence s2 v2 = Silence (s1 ++ s2) (v1 ++ v2)

instance Monoid Silence where
    mempty = Silence [] []

-- | Tag expressions.
data TagExpr
    = TConc !Integer
    | TOffset !String !Integer
    deriving (Eq, Generic, Show)

instance Hashable TagExpr

-- | A parsed process carrying metadata @meta@.
type RawProcess meta = WithData meta (RawProcess' meta)

-- | 'RawProcess' with a source-location field.
type RawProcessFC = RawProcess '[LocKV]

-- | Parsed processes.
data RawProcess' meta
    = -- | Qubit discard: @disc(q1, x, y)@
      RDiscard [RawExpr meta]
    | -- | Internal step: @t : tau.P@
      RTau !(WithLoc TagExpr) (RawProcess meta)
    | -- | Synchronized internal step with two tags: @(t1, t2) : tau.P@
      RTau2 !(WithLoc TagExpr) !(WithLoc TagExpr) (RawProcess meta)
    | -- | Channel send: @t : c!e.P@ or @t : c:qubit!e.P@
      RSend !(WithLoc TagExpr) !(WithLoc String) !(Maybe (WithLoc Types)) (RawExpr meta) (RawProcess meta)
    | -- | Channel receive: @t : c?x.P@ or @t : c:qubit?x.P@
      RRecv !(WithLoc TagExpr) !(WithLoc String) !(Maybe (WithLoc Types)) !(WithLoc String) (RawProcess meta)
    | -- | Quantum operator application: @t : CNOT(q1, q2).P@
      RApply !(WithLoc TagExpr) !(WithLoc String) [RawExpr meta] (RawProcess meta)
    | -- | Quantum measurement: @t : M201(q1, q2 > x).P@
      RMeas !(WithLoc TagExpr) !(WithLoc String) [RawExpr meta] !(WithLoc String) (RawProcess meta)
    | -- | Conditional: @if e then P1 else P2@
      RITE (RawExpr meta) (RawProcess meta) (RawProcess meta)
    | -- | Non-deterministic choice: @P1 + P2@
      RSum (RawProcess meta) (RawProcess meta)
    | -- | Parallel composition: @P1 || P2@
      RPar (RawProcess meta) (RawProcess meta)
    | -- | Channel restriction: @P \\ c@
      RRestr (RawProcess meta) !(WithLoc String)
    | -- | Process call: @\@Proc[qb0, qb1](42)@ or @\@Proc[qb0](c) with base=100@
      RRef !String ![RawExpr meta] ![RawExpr meta] ![RawCallModifier meta]
    deriving (Eq, Generic, Show)

instance Hashable (RawProcess' meta)

-- | A parsed top-level declaration carrying metadata @meta@.
type RawDeclaration meta = WithData meta (RawDeclaration' meta)

-- | 'RawDeclaration' with a source-location field.
type RawDeclarationFC = RawDeclaration '[LocKV]

-- | A parsed process declaration.
data RawDeclaration' meta
    = RawDecl
        -- | Declaration name.
        !(WithLoc String)
        -- | Qubit parameters, e.g. @[q0, q1]@.
        ![WithLoc String]
        -- | Channel parameters with their value types, e.g. @(c : chan int)@.
        ![(WithLoc String, WithLoc Types)]
        -- | Value parameters with their types, e.g. @(x : int)@.
        ![(WithLoc String, WithLoc Types)]
        -- | Declaration modifiers, e.g. @with tag_offset(base)@.
        ![RawDeclModifier meta]
        -- | Process body.
        (RawProcess meta)
    deriving (Eq, Generic, Show)

instance Hashable (RawDeclaration' meta)

declCopyName :: String -> Int -> String
declCopyName name n = name ++ "#" ++ show n

declDisplayName :: String -> String
declDisplayName = takeWhile (/= '#')

-- | A parse-phase modifier on a process declaration.
data RawDeclModifier meta
    = -- | Declares a tag-offset base: @tag_offset(name)@.
      RawTagOffset !(WithLoc String)
    deriving (Eq, Generic, Show)

instance Hashable (RawDeclModifier meta)

-- | 'RawDeclModifier' with a source-location field.
type RawDeclModifierFC = RawDeclModifier '[LocKV]

-- | An elaborated declaration modifier.
data DeclModifier
    = -- | Tag-offset base.
      TagOffsetMod !String ![(Integer, Integer)]
    deriving (Eq, Show)

-- | A parse-phase modifier at a process call site.
data RawCallModifier meta
    = -- | Supplies a concrete value for a tag-offset base: @name=expr@.
      RawTagOffsetValue !String !(RawExpr meta)
    deriving (Eq, Generic, Show)

instance Hashable (RawCallModifier meta)

-- | 'RawCallModifier' with a source-location field.
type RawCallModifierFC = RawCallModifier '[LocKV]

-- | A resolved channel.
data Channel = Channel
    { chname :: !String
    , chtype :: !Types
    }
    deriving (Eq, Generic, Show)

instance Hashable Channel

-- | A resolved variable.
data Var = Var
    { varname :: !String
    , vartype :: !Types
    }
    deriving (Eq, Generic, Show)

instance Hashable Var

-- | A density operator represented as a complex matrix.
newtype DensityOp = DensityOp {unDensityOp :: Matrix C} deriving (Show, Generic)

instance Eq DensityOp where
    DensityOp a == DensityOp b = unsafeMatrixEq a b

unsafeMatrixEq :: Matrix C -> Matrix C -> Bool
unsafeMatrixEq a b = cols a == cols b && VS.length xs == VS.length ys && go 0
  where
    xs = VS.unsafeCast (flatten a) :: VS.Vector Double
    ys = VS.unsafeCast (flatten b) :: VS.Vector Double
    n = VS.length xs
    go !i = i >= n || (VS.unsafeIndex xs i == VS.unsafeIndex ys i && go (i + 1))

-- | A superoperator in Kraus representation.
newtype Superoperator = Superoperator {krausOps :: [Matrix C]} deriving (Show, Generic)

instance Eq Superoperator where
    Superoperator ks == Superoperator ks' = length ks == length ks' && and (zipWith unsafeMatrixEq ks ks')

-- | A named quantum operator in Kraus form.
data Operator = Operator
    { opName :: !String
    , opDim :: !Integer
    , opKraus :: !Superoperator
    }
    deriving (Eq, Generic)

instance Hashable Operator where
    hashWithSalt salt (Operator name _ _) = hashWithSalt salt name

instance Show Operator where
    show (Operator name dim _) = printf "Operator {name=%s,dim=%d,op=[...]}" name dim

-- | A named quantum measurement in Kraus form.
data Measure = Measure
    { measName :: !String
    , measSize :: !Integer
    , measKraus :: !Superoperator
    }
    deriving (Eq, Generic)

instance Hashable Measure where
    hashWithSalt salt (Measure name _ _) = hashWithSalt salt name

instance Show Measure where
    show (Measure name dim _) = printf "Operator {name=%s,dim=%d,meas=[...]}" name dim

-- | The runtime environment of quantum operators and measurements.
data OpsMap = OpsMap
    { opSOP :: !(HashMap String Operator)
    , opMeas :: !(HashMap String Measure)
    }
    deriving (Eq, Show)

-- | A qubit expression.
data QExpr
    = -- | A qubit variable, e.g. @q0@.
      QVar !String
    | -- | A constant qubit index, e.g. @qb3@.
      QConst !Integer
    deriving (Eq, Generic, Show)

instance Hashable QExpr

instance Ord QExpr where
    compare (QConst i) (QConst j) = compare i j
    compare (QVar i) (QVar j) = compare i j
    compare (QConst _) _ = LT
    compare (QVar _) _ = GT

-- | An elaborated expression.
type Expr meta = WithData meta (Expr' meta)

-- | 'Expr' with a source-location field.
type ExprFC = Expr '[LocKV]

-- | Elaborated expression nodes.
data Expr' meta
    = -- | Integer literal.
      IntLit !Integer
    | -- | Boolean literal.
      BoolLit !Bool
    | -- | Qubit literal (constant qubit index).
      QubitLit !Integer
    | -- | Variable reference (resolved by the type-checker).
      Ref !Var
    | -- | Equality test.
      Eq_ (Expr meta) (Expr meta)
    | -- | Less-than-or-equal.
      LEq (Expr meta) (Expr meta)
    | -- | Logical conjunction.
      And (Expr meta) (Expr meta)
    | -- | Logical disjunction.
      Or (Expr meta) (Expr meta)
    | -- | Logical negation.
      Not (Expr meta)
    | -- | Integer addition.
      Plus (Expr meta) (Expr meta)
    | -- | Integer subtraction.
      Sub (Expr meta) (Expr meta)
    | -- | Integer multiplication.
      Mul (Expr meta) (Expr meta)
    | -- | Integer negation.
      Neg (Expr meta)
    | -- | Bitwise AND.
      BAnd (Expr meta) (Expr meta)
    | -- | Bitwise OR.
      BOr (Expr meta) (Expr meta)
    | -- | Bitwise XOR.
      BXor (Expr meta) (Expr meta)
    | -- | Integer less-than.
      Lt (Expr meta) (Expr meta)
    deriving (Eq, Generic, Show)

instance Hashable (Expr' meta)

-- | Returns 'True' if the expression is a literal value.
isValue :: Expr meta -> Bool
isValue (WD _ IntLit{}) = True
isValue (WD _ BoolLit{}) = True
isValue (WD _ QubitLit{}) = True
isValue _ = False

-- | An evaluated expression carrying metadata @meta@.
type Value meta = WithData meta Value'

-- | 'Value' with a source-location field.
type ValueFC = Value '[LocKV]

-- | A fully-evaluated or partially-bound expression value.
data Value'
    = -- | Evaluated integer literal.
      VIntLit !Integer
    | -- | Evaluated boolean literal.
      VBoolLit !Bool
    | -- | Evaluated qubit literal (constant qubit index).
      VQubitLit !Integer
    | -- | A bound but not yet substituted variable reference.
      VRef !Var
    deriving (Eq, Generic, Show)

instance Hashable Value'

-- | An elaborated process.
type Process meta = WithData meta (Process' meta)

-- | 'Process' with a source-location field.
type ProcessFC = Process '[LocKV]

-- | Elaborated process nodes.
data Process' meta
    = -- | Qubit discard: @disc(q1, q2, ...)@
      Discard ![WithLoc QExpr]
    | -- | Internal step: @t : tau.P@
      Tau !(WithLoc Integer) (Process meta)
    | -- | Synchronized internal step with two tags: @(t1, t2) : tau.P@
      Tau2 !(WithLoc Integer) !(WithLoc Integer) (Process meta)
    | -- | Channel send: @t : c!e.P@
      Send !(WithLoc Integer) !(WithLoc Channel) !(Expr meta) (Process meta)
    | -- | Channel receive: @t : c?x.P@
      Recv !(WithLoc Integer) !(WithLoc Channel) !(WithLoc Var) (Process meta)
    | -- | Quantum operator application: @t : OP(q1, q2).P@
      Apply !(WithLoc Integer) !(WithLoc Operator) ![WithLoc QExpr] (Process meta)
    | -- | Quantum measurement: @t : M(q1, q2 > x).P@
      Meas !(WithLoc Integer) !(WithLoc Measure) ![WithLoc QExpr] !(WithLoc Var) (Process meta)
    | -- | Conditional: @if e then P1 else P2@
      ITE !(Expr meta) (Process meta) (Process meta)
    | -- | Non-deterministic choice: @P1 + P2 + ...@ (n-ary, normalized)
      Sum [Process meta]
    | -- | Parallel composition: @P1 || P2 || ...@ (n-ary, normalized)
      Par [Process meta]
    | -- | Channel restriction: @P \\ c@
      Restr (Process meta) !(WithLoc Channel)
    | -- | Process call: @Proc[q0, q1](c, 42)@
      Call !(WithLoc String) ![WithLoc QExpr] ![WithLoc Channel] ![Expr meta]
    deriving (Eq, Generic, Show)

instance Hashable (Process' meta)

{- | Minimum number of qubits required to evaluate an expression, determined
by the largest constant qubit index it references. Returns 0 for
expressions with no qubit literals.
-}
minSizeExpr :: Expr meta -> Integer
minSizeExpr (WD _ e) = case e of
    IntLit _ -> 0
    BoolLit _ -> 0
    QubitLit k -> k + 1
    Ref _ -> 0
    Eq_ lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    LEq lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    And lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    Or lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    Not lhs -> minSizeExpr lhs
    Plus lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    Sub lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    Mul lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    Neg lhs -> minSizeExpr lhs
    BAnd lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    BOr lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    BXor lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)
    Lt lhs rhs -> max (minSizeExpr lhs) (minSizeExpr rhs)

{- | Minimum number of qubits required by a list of qubit arguments.
'QVar' arguments contribute nothing (their index is unknown statically).
-}
minSizeArgs :: [WithLoc QExpr] -> Integer
minSizeArgs [] = 0
minSizeArgs xs = (+ 1) . maximum . fmap (\case WD _ (QVar _) -> -1; WD _ (QConst k) -> k) $ xs

{- | Minimum number of qubits required to run a process, determined
by the largest constant qubit index it accesses anywhere in its tree.
-}
minSize :: Process meta -> Integer
minSize (WD _ p) = case p of
    Discard args -> minSizeArgs args
    Tau _ cont -> minSize cont
    Tau2 _ _ cont -> minSize cont
    Send _ _ v cont -> max (minSizeExpr v) (minSize cont)
    Recv _ _ _ cont -> minSize cont
    Apply _ _ args cont -> max (minSizeArgs args) (minSize cont)
    Meas _ _ args _ cont -> max (minSizeArgs args) (minSize cont)
    ITE guard lhs rhs -> maximum [minSizeExpr guard, minSize lhs, minSize rhs]
    Sum ps -> foldl max 0 (minSize <$> ps)
    Par ps -> foldl max 0 (minSize <$> ps)
    Restr proc _ -> minSize proc
    Call _ qargs _ _ -> minSizeArgs qargs

{- | A metadata-independent structural fingerprint for a process, used to
impose a stable total order on processes regardless of source location.
The fingerprint encodes the constructor sequence and action tags.
-}
procFingerprint :: Process meta -> [Int]
procFingerprint (WD _ p) = case p of
    Discard args -> [0, length args]
    Tau tag cont -> 1 : fromIntegral tag.val : procFingerprint cont
    Tau2 t1 t2 cont -> 2 : fromIntegral t1.val : fromIntegral t2.val : procFingerprint cont
    Send tag ch _ cont -> 3 : fromIntegral tag.val : hash ch.val.chname : procFingerprint cont
    Recv tag ch _ cont -> 4 : fromIntegral tag.val : hash ch.val.chname : procFingerprint cont
    Apply tag op _ cont -> case op.val of
        Operator n _ _ -> 5 : fromIntegral tag.val : hash n : procFingerprint cont
    Meas tag m _ _ cont -> case m.val of
        Measure n _ _ -> 6 : fromIntegral tag.val : hash n : procFingerprint cont
    ITE _ lhs rhs -> 7 : lengthTagged lhs ++ lengthTagged rhs
    Sum ps -> 8 : length ps : concatMap lengthTagged ps
    Par ps -> 9 : length ps : concatMap lengthTagged ps
    Restr cont ch -> 10 : hash ch.val.chname : procFingerprint cont
    Call name qargs chargs exprArgs ->
        11
            : hash name.val
            : length qargs
            : length chargs
            : length exprArgs
            : map (hash . (.val)) qargs
            ++ map (hash . (.val)) chargs
  where
    lengthTagged sub = let f = procFingerprint sub in length f : f

-- | Sort a list of processes into a canonical order using 'procFingerprint'.
sortProcs :: [Process meta] -> [Process meta]
sortProcs = sortOn procFingerprint

-- | Flatten one level of nested 'Par' constructors in a list of processes.
flattenPar :: [Process meta] -> [Process meta]
flattenPar = concatMap $ \p -> case p.val of
    Par ps -> ps
    _ -> [p]

-- | Flatten one level of nested 'Sum' constructors in a list of processes.
flattenSum :: [Process meta] -> [Process meta]
flattenSum = concatMap $ \p -> case p.val of
    Sum ps -> ps
    _ -> [p]

-- | Construct a normalized 'Par' node.
mkPar :: [Process meta] -> Process' meta
mkPar = Par . sortProcs . flattenPar

-- | Construct a normalized 'Sum' node.
mkSum :: [Process meta] -> Process' meta
mkSum = Sum . sortProcs . flattenSum

-- | An elaborated process declaration.
data Declaration meta = Declaration
    { decQArgs :: ![WithLoc String]
    -- ^ Qubit parameter names.
    , decChArgs :: ![(WithLoc String, WithLoc Types)]
    -- ^ Channel parameters with their value types.
    , decVArgs :: ![(WithLoc String, WithLoc Types)]
    -- ^ Value parameters with their types.
    , decModifiers :: ![DeclModifier]
    -- ^ Elaborated declaration modifiers (e.g. tag-offset base mappings).
    , decBody :: !(Process meta)
    -- ^ Process body.
    }
    deriving (Eq, Show)

-- | 'Declaration' with source-location metadata.
type DeclarationFC = Declaration '[LocKV]

-- | Map from declaration names to their elaborated 'Declaration' bodies.
type DeclMap meta = HashMap String (Declaration meta)

-- | 'DeclMap' with source-location metadata.
type DeclMapFC = DeclMap '[LocKV]
