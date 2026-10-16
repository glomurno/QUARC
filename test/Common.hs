{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Helpers shared by the library test specs.
module Common where

import Control.Monad (unless)
import Data.Complex (Complex ((:+)))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.WithData
import GHC.Generics
import Numeric.LinearAlgebra (asColumn, asRow, ident, kronecker, outer, scale, (><))

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (ParseErrorBundle, eof)

import LqCCS.Parser (pDeclaration, pExpr, pProcess, runQuarcParser)
import LqCCS.Parser.File (InputFileResult, parseInput)
import LqCCS.QDist (QState (..))
import LqCCS.Quantum (minimalMeasures, minimalOps, minus, one, phiplus, plus, zero)
import LqCCS.Tagging (retagProcess)
import LqCCS.Typecheck (TypecheckError, infer, inferExpr)
import LqCCS.Types

fc :: Int -> Int -> FC
fc a b = FC "" (1, a) (1, b)

wd :: a -> WithLoc a
wd = WD (Cons (fc 1 1) Nil) -- Equality on 'WithData' ignores metadata.

firstOf :: (HasCallStack) => String -> [a] -> IO a
firstOf _ (a : _) = pure a
firstOf what [] = expectationFailure ("expected at least one " ++ what) >> error "unreachable"

onlyOf :: (HasCallStack) => String -> [a] -> IO a
onlyOf _ [a] = pure a
onlyOf what as = expectationFailure ("expected exactly one " ++ what ++ ", got " ++ show (length as)) >> error "unreachable"

orFail :: (Show e) => String -> Either e a -> a
orFail msg = either (\e -> error (msg ++ ": " ++ show e)) id

parseE :: Text -> RawExprFC
parseE = orFail "unexpected parsing error" . runQuarcParser (pExpr <* eof) ""

parseP :: Text -> RawProcessFC
parseP = orFail "unexpected parsing error" . runQuarcParser (pProcess <* eof) ""

parseD :: Text -> RawDeclarationFC
parseD = orFail "unexpected parsing error" . runQuarcParser (pDeclaration <* eof) ""

parseFile :: String -> Either String InputFileResult
parseFile = parseInput minimalOps minimalMeasures mempty

opsMap :: OpsMap
opsMap = OpsMap minimalOps minimalMeasures

typecheckE :: Text -> Either TypecheckError (ExprFC, Types)
typecheckE = inferExpr opsMap . parseE

typecheckP :: Text -> Either TypecheckError ProcessFC
typecheckP = infer opsMap [] . parseP

typecheckD :: [Text] -> Text -> Either TypecheckError ProcessFC
typecheckD declsT procT = infer opsMap (parseD <$> declsT) (parseP procT)

parseInferE :: Text -> (ExprFC, Types)
parseInferE = orFail "unexpected typechecking error" . typecheckE

parseInferP :: Text -> ProcessFC
parseInferP = orFail "unexpected typechecking error" . typecheckP

parseInferTaggedP :: Text -> ProcessFC
parseInferTaggedP = orFail "unexpected typechecking error" . infer opsMap [] . fst . retagProcess [] . parseP

empty, i1, zero1, zero2, plus1, minus1, one1, one2, zeroone, onezero, phiplus2 :: QState
empty = QState 0 (DensityOp ((0 >< 0) []))
i1 = QState 1 (DensityOp (ident 2 / 2))
zero1 = QState 1 (DensityOp $ outer zero zero)
zero2 = QState 2 (DensityOp $ kronecker (asColumn zero) (asColumn zero) <> kronecker (asRow zero) (asRow zero))
plus1 = QState 1 (DensityOp $ outer plus plus)
minus1 = QState 1 (DensityOp $ outer minus minus)
zeroone = QState 2 (DensityOp $ kronecker (asColumn zero) (asColumn one) <> kronecker (asRow zero) (asRow one))
onezero = QState 2 (DensityOp $ kronecker (asColumn one) (asColumn zero) <> kronecker (asRow one) (asRow zero))
one1 = QState 1 (DensityOp $ outer one one)
one2 = QState 2 (DensityOp $ kronecker (asColumn one) (asColumn one) <> kronecker (asRow one) (asRow one))
phiplus2 = QState 2 (DensityOp $ outer phiplus phiplus)

scaleQS :: Double -> QState -> QState
scaleQS p (QState dim rho) = QState dim (DensityOp $ scale (p :+ 0.0) rho.unDensityOp)

checkProp :: Property -> IO ()
checkProp p = do
    result <- H.check p
    result `shouldBe` True

-- A random process with at most one qubit
genProcess :: H.Gen (QState, Text)
genProcess = do
    withQubit <- Gen.bool
    qs <- if withQubit then Gen.element [zero1, one1, plus1] else pure empty
    budget <- Gen.int (Range.linear 3 14)
    (qs,) . T.pack <$> genProc budget withQubit []

-- A process with at most n prefixes on any path and at most one qubit
genProc :: Int -> Bool -> [String] -> H.Gen String
genProc n held vars
    | n <= 0 = pure (stop held)
    | otherwise =
        Gen.frequency $
            [(1, pure (stop held)) | n <= 2]
                ++ [ (8, genPrefix n held vars)
                   , (2, genSum n held vars)
                   , (2, Gen.bool >>= \left -> binary "||" (half (held && left) vars) (half (held && not left) vars))
                   , (1, restr)
                   ]
                ++ [(2, ite) | not (null vars)]
  where
    half = genProc ((n - 1) `div` 2)
    sub = genProc (n - 1)
    ite = do
        cond <- (++ " = 0") <$> Gen.element vars
        (\a b -> paren ("if " ++ cond ++ " then " ++ paren a ++ " else " ++ paren b)) <$> sub held vars <*> sub held vars
    restr = do
        ch <- Gen.element ["c", "r"]
        (\p -> paren (paren p ++ " \\ " ++ ch)) <$> sub held vars

genSum :: Int -> Bool -> [String] -> H.Gen String
genSum n held vars = binary "+" (summand held vars) (summand held vars)
  where
    m = n - 1
    summand h vs
        | m <= 0 = pure (stop h)
        | otherwise = Gen.frequency [(1, pure (stop h)), (4, genPrefix m h vs), (1, genSum m h vs)]

genPrefix :: Int -> Bool -> [String] -> H.Gen String
genPrefix n held vars =
    Gen.choice $
        [ act "tau" (sub held vars)
        , Gen.element ["c!0", "c!1", "d!0", "r:bit!0", "r:bit!1"] >>= \a -> act a (sub held vars)
        , act ("r:bit?" ++ x) (sub held (x : vars))
        ]
            ++ [Gen.element vars >>= \v -> act ("m:bit!" ++ v) (sub held vars) | not (null vars)]
            ++ [ g
               | held
               , g <-
                    [ Gen.element ["X", "H"] >>= \op -> act (op ++ "(qb0)") (sub held vars)
                    , Gen.element ["M01", "MH"] >>= \op -> act (op ++ "(qb0>" ++ x ++ ")") (sub held (x : vars))
                    , act "q:qubit!qb0" (sub False vars)
                    ]
               ]
  where
    act a k = (\cont -> a ++ "." ++ cont) <$> k
    x = "x" ++ show n
    sub = genProc (n - 1)

stop :: Bool -> String
stop held = if held then "disc(qb0)" else "disc()"

paren :: String -> String
paren s = "(" ++ s ++ ")"

binary :: String -> H.Gen String -> H.Gen String -> H.Gen String
binary op = liftA2 (\a b -> paren (paren a ++ " " ++ op ++ " " ++ paren b))

class HasSpans a where
    spans :: a -> [(String, FC)]
    default spans :: (Generic a, GHasSpans (Rep a)) => a -> [(String, FC)]
    spans = gspans . from

    nodeLabel :: a -> String
    default nodeLabel :: (Generic a, GHasSpans (Rep a)) => a -> String
    nodeLabel = gconName . from

class GHasSpans f where
    gspans :: f p -> [(String, FC)]
    gconName :: f p -> String

instance GHasSpans V1 where
    gspans _ = []
    gconName _ = ""

instance GHasSpans U1 where
    gspans _ = []
    gconName _ = ""

instance (GHasSpans f, GHasSpans g) => GHasSpans (f :+: g) where
    gspans (L1 x) = gspans x
    gspans (R1 x) = gspans x
    gconName (L1 x) = gconName x
    gconName (R1 x) = gconName x

instance (GHasSpans f, GHasSpans g) => GHasSpans (f :*: g) where
    gspans (x :*: y) = gspans x ++ gspans y
    gconName _ = ""

instance (HasSpans c) => GHasSpans (K1 i c) where
    gspans (K1 x) = spans x
    gconName _ = ""

instance (GHasSpans f) => GHasSpans (M1 D c f) where
    gspans (M1 x) = gspans x
    gconName (M1 x) = gconName x

instance (GHasSpans f, Constructor c) => GHasSpans (M1 C c f) where
    gspans (M1 x) = gspans x
    gconName = conName

instance (GHasSpans f) => GHasSpans (M1 S c f) where
    gspans (M1 x) = gspans x
    gconName _ = ""

instance (HasSpans a) => HasSpans (WithLoc a) where
    spans (WD m v) = (nodeLabel v, getLoc (WD m v)) : spans v
    nodeLabel = nodeLabel . val

instance HasSpans Integer where
    spans _ = []
    nodeLabel = show

instance HasSpans Int where
    spans _ = []
    nodeLabel = show

instance HasSpans Bool where
    spans _ = []
    nodeLabel = show

instance {-# OVERLAPPING #-} HasSpans String where
    spans _ = []
    nodeLabel = show

instance (HasSpans a) => HasSpans [a] where
    spans = concatMap spans
    nodeLabel _ = "list"

instance (HasSpans a) => HasSpans (Maybe a)
instance (HasSpans a, HasSpans b) => HasSpans (a, b)
instance HasSpans Types
instance HasSpans TagExpr
instance HasSpans (RawExpr' '[LocKV])
instance HasSpans (RawProcess' '[LocKV])
instance HasSpans (RawDeclaration' '[LocKV])
instance HasSpans (RawDeclModifier '[LocKV])
instance HasSpans (RawCallModifier '[LocKV])

shouldParseAt :: (HasCallStack, Eq a, Show a, HasSpans a) => Either (ParseErrorBundle Text Void) a -> a -> Expectation
shouldParseAt r expected = do
    r `shouldParse` expected
    let actualSpans = either (const []) spans r
        mismatches =
            [ "  #" ++ show i ++ " " ++ lbl ++ ": expected " ++ showFC e ++ ", got " ++ showFC a
            | (i, (lbl, e), (_, a)) <- zip3 [0 :: Int ..] (spans expected) actualSpans
            , e /= a
            ]
    unless (null mismatches) $
        expectationFailure ("source spans differ:\n" ++ unlines mismatches)

showFC :: FC -> String
showFC (FC _ (l1, c1) (l2, c2)) = show l1 ++ ":" ++ show c1 ++ "-" ++ show l2 ++ ":" ++ show c2
showFC NoFC = "no location"
