{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.ParserSpec (spec) where

import Data.Either (isLeft)
import Data.List (nub, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.WithData
import Text.Megaparsec

import Common (HasSpans (..), checkProp, genProcess, opsMap, shouldParseAt)
import Hedgehog (Gen, PropertyT, annotate, assert, evalEither, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn)

import LqCCS.Parser (pDeclaration, pExpr, pProcess, pType, runQuarcParser)
import LqCCS.Pretty (AutoTagStyle (..), asciiDecl, asciiExpr, asciiProcess, asciiProcessWith)
import LqCCS.Tagging (retagProcess)
import LqCCS.Typecheck (DeclResult (..), infer, inferDeclarations, inferExpr)
import LqCCS.Types

spec :: Spec
spec = do
    typeSpec
    exprSpec
    processSpec
    declarationSpec
    roundTripSpec

fname :: String
fname = "testfile"

fc :: Int -> Int -> FC
fc a b = FC fname (1, a) (1, b)

wd :: Int -> Int -> a -> WithLoc a
wd s e = WD (Cons (fc s e) Nil)

int :: Int -> Integer -> RawExprFC
int s n = wd s (s + length (show n) - 1) (RIntLit n)

bool :: Int -> Bool -> RawExprFC
bool s b = wd s (s + if b then 3 else 4) (RBoolLit b)

qb :: Int -> Integer -> RawExprFC
qb s n = wd s (s + length (show n) + 1) (RQubitLit n)

var :: Int -> String -> RawExprFC
var s x = wd s (s + length x - 1) (RVar x)

name :: Int -> String -> WithLoc String
name s x = wd s (s + length x - 1) x

tag :: Int -> Integer -> WithLoc TagExpr
tag s n = wd s (s + length (show n)) (TConc n)

disc :: Int -> Int -> [RawExprFC] -> RawProcessFC
disc s e es = wd s e (RDiscard es)

tau :: Int -> Int -> WithLoc TagExpr -> RawProcessFC -> RawProcessFC
tau s e t k = wd s e (RTau t k)

tau2 :: Int -> Int -> WithLoc TagExpr -> WithLoc TagExpr -> RawProcessFC -> RawProcessFC
tau2 s e t1 t2 k = wd s e (RTau2 t1 t2 k)

send :: Int -> Int -> WithLoc TagExpr -> WithLoc String -> Maybe (WithLoc Types) -> RawExprFC -> RawProcessFC -> RawProcessFC
send s e t c ty v k = wd s e (RSend t c ty v k)

recv :: Int -> Int -> WithLoc TagExpr -> WithLoc String -> Maybe (WithLoc Types) -> WithLoc String -> RawProcessFC -> RawProcessFC
recv s e t c ty x k = wd s e (RRecv t c ty x k)

apply :: Int -> Int -> WithLoc TagExpr -> WithLoc String -> [RawExprFC] -> RawProcessFC -> RawProcessFC
apply s e t op args k = wd s e (RApply t op args k)

meas :: Int -> Int -> WithLoc TagExpr -> WithLoc String -> [RawExprFC] -> WithLoc String -> RawProcessFC -> RawProcessFC
meas s e t m args x k = wd s e (RMeas t m args x k)

ite :: Int -> Int -> RawExprFC -> RawProcessFC -> RawProcessFC -> RawProcessFC
ite s e g p q = wd s e (RITE g p q)

sm :: Int -> Int -> RawProcessFC -> RawProcessFC -> RawProcessFC
sm s e p q = wd s e (RSum p q)

par :: Int -> Int -> RawProcessFC -> RawProcessFC -> RawProcessFC
par s e p q = wd s e (RPar p q)

restr :: Int -> Int -> RawProcessFC -> WithLoc String -> RawProcessFC
restr s e p c = wd s e (RRestr p c)

parseT :: Text -> Either (ParseErrorBundle Text Void) (WithLoc Types)
parseT = runQuarcParser (pType <* eof) fname

parseE :: Text -> Either (ParseErrorBundle Text Void) RawExprFC
parseE = runQuarcParser (pExpr <* eof) fname

parseP :: Text -> Either (ParseErrorBundle Text Void) RawProcessFC
parseP = runQuarcParser (pProcess <* eof) fname

typeSpec :: Spec
typeSpec =
    describe "type parser" $ do
        it "can parse bit" $
            parseT "bit" `shouldParseAt` wd 1 3 (TBits 1)
        it "can parse bits" $ checkProp $ property $ do
            n <- forAll $ Gen.int (Range.linear 1 100000)
            let src = "bits " <> T.pack (show n)
            let expected = wd 1 (T.length src) (TBits n)
            parseT src === Right expected
            fmap spans (parseT src) === Right (spans expected)
        it "rejects bits of zero or negative width" $ checkProp $ property $ do
            n <- forAll $ Gen.int (Range.linearFrom 0 (-100000) 0)
            assert $ isLeft $ parseT ("bits " <> T.pack (show n))
        it "can parse quantum" $
            parseT "qubit" `shouldParseAt` wd 1 5 TQuant
        it "can parse boolean" $
            parseT "bool" `shouldParseAt` wd 1 4 TBool

exprSpec :: Spec
exprSpec =
    describe "expression parser" $ do
        it "can parse integer literals" $ do
            parseE "42 " `shouldParseAt` int 1 42
            parseE "- 23" `shouldParseAt` wd 1 4 (RNeg (int 3 23))
        it "can parse binary literals" $ do
            parseE "0b1010" `shouldParseAt` wd 1 6 (RIntLit 10)
            parseE "0b0" `shouldParseAt` wd 1 3 (RIntLit 0)
        it "can parse boolean literals" $ do
            parseE "true" `shouldParseAt` bool 1 True
            parseE "false" `shouldParseAt` bool 1 False
        it "can parse qubit literals" $ do
            parseE "qb0" `shouldParseAt` qb 1 0
            parseE "q3" `shouldParseAt` var 1 "q3"
            parseE `shouldFailOn` "qb-1"
        it "can parse variables" $ do
            parseE "foo" `shouldParseAt` var 1 "foo"
            parseE "foo32" `shouldParseAt` var 1 "foo32"
            parseE "fo_o32" `shouldParseAt` var 1 "fo_o32"
            parseE `shouldFailOn` "3foo"
            parseE `shouldFailOn` "_foo"
        it "can parse parenthesis" $ do
            parseE "(42)" `shouldParseAt` int 2 42
            parseE "((42))" `shouldParseAt` int 3 42
        it "can parse operations" $ do
            parseE "42 + 2" `shouldParseAt` wd 1 6 (RPlus (int 1 42) (int 6 2))
            parseE "42 - 2" `shouldParseAt` wd 1 6 (RSub (int 1 42) (int 6 2))
            parseE "42 = 2" `shouldParseAt` wd 1 6 (REq (int 1 42) (int 6 2))
            parseE "42 <= 2" `shouldParseAt` wd 1 7 (RLEq (int 1 42) (int 7 2))
            parseE "42 < 2" `shouldParseAt` wd 1 6 (RLt (int 1 42) (int 6 2))
            parseE "3 & 5" `shouldParseAt` wd 1 5 (RBAnd (int 1 3) (int 5 5))
            parseE "3 | 5" `shouldParseAt` wd 1 5 (RBOr (int 1 3) (int 5 5))
            parseE "3 ^ 5" `shouldParseAt` wd 1 5 (RBXor (int 1 3) (int 5 5))
            parseE "true /\\ false" `shouldParseAt` wd 1 13 (RAnd (bool 1 True) (bool 9 False))
            parseE "-42" `shouldParseAt` wd 1 3 (RNeg (int 2 42))
            parseE "!x" `shouldParseAt` wd 1 2 (RNot (var 2 "x"))
            parseE "1 + (-2)" `shouldParseAt` wd 1 8 (RPlus (int 1 1) (wd 6 7 (RNeg (int 7 2))))
        it "can handle precedence" $ do
            parseE "1+2+3" `shouldParseAt` wd 1 5 (RPlus (wd 1 3 (RPlus (int 1 1) (int 3 2))) (int 5 3))
            parseE "(1+2)+3" `shouldParseAt` wd 1 7 (RPlus (wd 2 4 (RPlus (int 2 1) (int 4 2))) (int 7 3))
            parseE "-1+3" `shouldParseAt` wd 1 4 (RPlus (wd 1 2 (RNeg (int 2 1))) (int 4 3))
            parseE "true /\\ !false /\\ true" `shouldParseAt` wd 1 22 (RAnd (bool 1 True) (wd 9 22 (RAnd (wd 9 14 (RNot (bool 10 False))) (bool 19 True))))
            parseE "1=2/\\3<=4" `shouldParseAt` wd 1 9 (RAnd (wd 1 3 (REq (int 1 1) (int 3 2))) (wd 6 9 (RLEq (int 6 3) (int 9 4))))
            parseE `shouldFailOn` "1=2=3"
            parseE `shouldFailOn` "1<=2<=3"
            parseE `shouldFailOn` "1<2<3"
        it "rejects prefix operators directly after binary operators" $ do
            parseE `shouldFailOn` "1 - -2"
            parseE `shouldFailOn` "1 + -2"
            parseE `shouldFailOn` "1 * -2"

processSpec :: Spec
processSpec =
    describe "process parser" $ do
        it "can parse discard" $ do
            parseP "disc()" `shouldParseAt` disc 1 6 []
            parseP "disc(x)" `shouldParseAt` disc 1 7 [var 6 "x"]
            parseP "disc(x,y)" `shouldParseAt` disc 1 9 [var 6 "x", var 8 "y"]
            parseP `shouldFailOn` "disc(x;y)"
            parseP `shouldFailOn` "Disc(x,y)"
        it "can parse silent action" $ do
            parseP "t1 : tau.disc()" `shouldParseAt` tau 1 8 (tag 1 1) (disc 10 15 [])
            parseP "t2 : tau . disc()" `shouldParseAt` tau 1 8 (tag 1 2) (disc 12 17 [])
            parseP "t1 : tau.t2 : tau.disc(x)" `shouldParseAt` tau 1 8 (tag 1 1) (tau 10 17 (tag 10 2) (disc 19 25 [var 24 "x"]))
            parseP `shouldFailOn` "tau"
            parseP "tau.disc()" `shouldSatisfy` (\case Right (WD _ (RTau _ _)) -> True; _ -> False)
            parseP `shouldFailOn` "1 : tau.disc()"
            parseP `shouldFailOn` "t1:Tau.disc()"
            parseP `shouldFailOn` "t1:Tau().disc()"
            parseP "t1,t2 :tau.disc()" `shouldParseAt` tau2 1 10 (tag 1 1) (tag 4 2) (disc 12 17 [])
            parseP "t1, t2:tau.disc()" `shouldParseAt` tau2 1 10 (tag 1 1) (tag 5 2) (disc 12 17 [])
            parseP `shouldFailOn` "1,t2:tau.disc()"
            parseP `shouldFailOn` "t1,2:tau.disc()"
            parseP `shouldFailOn` "t1;t2:tau.disc()"
            parseP `shouldFailOn` "t1,t2:Tau.disc()"
            parseP `shouldFailOn` "t1,t2:Tau().disc()"
        it "can parse measure" $ do
            parseP "t1:M(qb1>x).disc()" `shouldParseAt` meas 1 11 (tag 1 1) (name 4 "M") [qb 6 1] (name 10 "x") (disc 13 18 [])
            parseP "t1:M(qb1,q>x).disc()" `shouldParseAt` meas 1 13 (tag 1 1) (name 4 "M") [qb 6 1, var 10 "q"] (name 12 "x") (disc 15 20 [])
            parseP "t1:MH(x>y).disc()" `shouldParseAt` meas 1 10 (tag 1 1) (name 4 "MH") [var 7 "x"] (name 9 "y") (disc 12 17 [])
            parseP "t1:M01(x>y).disc()" `shouldParseAt` meas 1 11 (tag 1 1) (name 4 "M01") [var 8 "x"] (name 10 "y") (disc 13 18 [])
            parseP `shouldFailOn` "M(qb1>x)"
            parseP `shouldFailOn` "1:M(qb1>x)"
            parseP `shouldFailOn` "t1:M(x;y>z).disc()"
            parseP "t1:M(x,y,z).disc()" `shouldSatisfy` (\case Right (WD _ RMeas{}) -> False; _ -> True)
            parseP `shouldFailOn` "t1:M(x>z)"
            parseP `shouldFailOn` "t1:M(x>z)disc()"
            parseP "t1:M(x,y,z).disc()" `shouldSatisfy` (\case Right (WD _ RMeas{}) -> False; _ -> True)
        it "can parse apply operators" $ do
            parseP "t1:X(qb1,qb2).disc()" `shouldParseAt` apply 1 13 (tag 1 1) (name 4 "X") [qb 6 1, qb 10 2] (disc 15 20 [])
            parseP "t1:Qft(qb1,qb2).disc()" `shouldParseAt` apply 1 15 (tag 1 1) (name 4 "Qft") [qb 8 1, qb 12 2] (disc 17 22 [])
            parseP "t1:Q0t(qb1,qb2).disc()" `shouldParseAt` apply 1 15 (tag 1 1) (name 4 "Q0t") [qb 8 1, qb 12 2] (disc 17 22 [])
            parseP "X(qb1).disc()" `shouldSatisfy` (\case Right (WD _ (RApply _ _ _ _)) -> True; _ -> False)
            parseP `shouldFailOn` "1:X(qb1).disc()"
            parseP `shouldFailOn` "t1:0(x;y).disc()"
            parseP `shouldFailOn` "t1:X(x;y)disc()"
        it "can parse sendings" $ do
            parseP "t1:c!x.disc()" `shouldParseAt` send 1 6 (tag 1 1) (name 4 "c") Nothing (var 6 "x") (disc 8 13 [])
            parseP "t1:c:bit!x.disc()" `shouldParseAt` send 1 10 (tag 1 1) (name 4 "c") (Just (wd 6 8 (TBits 1))) (var 10 "x") (disc 12 17 [])
            parseP `shouldFailOn` "t1:c!x"
            parseP "c!x.disc()" `shouldSatisfy` (\case Right (WD _ (RSend _ _ _ _ _)) -> True; _ -> False)
            parseP `shouldFailOn` "1:c!x.disc()"
            parseP `shouldFailOn` "t1:c!x.P"
            parseP `shouldFailOn` "t1:c:!x"
        it "can parse receives" $ do
            parseP "t1:c?x.disc()" `shouldParseAt` recv 1 6 (tag 1 1) (name 4 "c") Nothing (name 6 "x") (disc 8 13 [])
            parseP "t1:c:bool?x.disc()" `shouldParseAt` recv 1 11 (tag 1 1) (name 4 "c") (Just (wd 6 9 TBool)) (name 11 "x") (disc 13 18 [])
            parseP `shouldFailOn` "t1:c?x"
            parseP "c?x.disc()" `shouldSatisfy` (\case Right (WD _ (RRecv _ _ _ _ _)) -> True; _ -> False)
            parseP `shouldFailOn` "1:c?x.disc()"
            parseP `shouldFailOn` "t1:c?x"
            parseP `shouldFailOn` "t1:c:?x.disc()"
        it "can handle parenthesis" $ do
            parseP "(disc())" `shouldParseAt` disc 2 7 []
            parseP "((disc()))" `shouldParseAt` disc 3 8 []
            parseP "(t1:tau.(disc()))" `shouldParseAt` tau 2 7 (tag 2 1) (disc 10 15 [])
        it "can parse non-deterministic choice" $ do
            parseP "disc() + disc()" `shouldParseAt` sm 1 15 (disc 1 6 []) (disc 10 15 [])
            parseP "(t1:c!1.disc()) + disc()" `shouldParseAt` sm 1 24 (send 2 7 (tag 2 1) (name 5 "c") Nothing (int 7 1) (disc 9 14 [])) (disc 19 24 [])
            parseP "(disc() + disc()) + disc()" `shouldParseAt` sm 1 26 (sm 2 16 (disc 2 7 []) (disc 11 16 [])) (disc 21 26 [])
        it "can parse parallel" $ do
            parseP "disc() || disc()" `shouldParseAt` par 1 16 (disc 1 6 []) (disc 11 16 [])
            parseP "(disc() || disc()) || disc()" `shouldParseAt` par 1 28 (par 2 17 (disc 2 7 []) (disc 12 17 [])) (disc 23 28 [])
        it "can parse ite" $ do
            parseP "if true then disc() else disc()" `shouldParseAt` ite 1 31 (bool 4 True) (disc 14 19 []) (disc 26 31 [])
            parseP "if true then disc() else if false then disc() else disc()" `shouldParseAt` ite 1 57 (bool 4 True) (disc 14 19 []) (ite 26 57 (bool 29 False) (disc 40 45 []) (disc 52 57 []))
        it "can parse restriction" $ do
            parseP "disc() \\ c" `shouldParseAt` restr 1 10 (disc 1 6 []) (name 10 "c")
            parseP "(disc() \\ c) \\ d" `shouldParseAt` restr 1 16 (restr 2 11 (disc 2 7 []) (name 11 "c")) (name 16 "d")
            parseP "disc() \\ c \\ d" `shouldParseAt` restr 1 14 (restr 1 10 (disc 1 6 []) (name 10 "c")) (name 14 "d")
        it "can parse complex processes" $ do
            parseP "t1:tau.disc() || disc()" `shouldParseAt` tau 1 6 (tag 1 1) (par 8 23 (disc 8 13 []) (disc 18 23 []))
            parseP "(t1:tau.disc()) || disc()" `shouldParseAt` par 1 25 (tau 2 7 (tag 2 1) (disc 9 14 [])) (disc 20 25 [])
            parseP "disc() || disc() + disc()" `shouldParseAt` par 1 25 (disc 1 6 []) (sm 11 25 (disc 11 16 []) (disc 20 25 []))
        it "can parse sugared parallel with a shared continuation" $ do
            parseP "(t1:tau || t2:tau).disc()" `shouldParseAt` par 1 25 (tau 2 7 (tag 2 1) (disc 20 25 [])) (tau 12 17 (tag 12 2) (disc 20 25 []))
            parseP "(t1:c!x || t2:c?y).disc()" `shouldParseAt` par 1 25 (send 2 7 (tag 2 1) (name 5 "c") Nothing (var 7 "x") (disc 20 25 [])) (recv 12 17 (tag 12 2) (name 15 "c") Nothing (name 17 "y") (disc 20 25 []))
            parseP "(t1:tau || t2:tau || t3:tau).disc()" `shouldParseAt` par 1 35 (tau 2 7 (tag 2 1) (disc 30 35 [])) (par 1 35 (tau 12 17 (tag 12 2) (disc 30 35 [])) (tau 22 27 (tag 22 3) (disc 30 35 [])))
            parseP "(t1:tau.t2:c!x || t3:tau).disc()" `shouldParseAt` par 1 32 (tau 2 7 (tag 2 1) (send 9 14 (tag 9 2) (name 12 "c") Nothing (var 14 "x") (disc 27 32 []))) (tau 19 24 (tag 19 3) (disc 27 32 []))
            parseP `shouldFailOn` "(t1:tau || t2:tau)"
        it "can parse sugared non-deterministic sum with a shared continuation" $ do
            parseP "(t1:tau + t2:tau).disc()" `shouldParseAt` sm 1 24 (tau 2 7 (tag 2 1) (disc 19 24 [])) (tau 11 16 (tag 11 2) (disc 19 24 []))
            parseP "(t1:c?x + t2:c?y).disc()" `shouldParseAt` sm 1 24 (recv 2 7 (tag 2 1) (name 5 "c") Nothing (name 7 "x") (disc 19 24 [])) (recv 11 16 (tag 11 2) (name 14 "c") Nothing (name 16 "y") (disc 19 24 []))
            parseP "(t1:tau + t2:tau + t3:tau).disc()" `shouldParseAt` sm 1 33 (tau 2 7 (tag 2 1) (disc 28 33 [])) (sm 1 33 (tau 11 16 (tag 11 2) (disc 28 33 [])) (tau 20 25 (tag 20 3) (disc 28 33 [])))
            parseP "(t1:c!x.t2:tau + t3:tau).disc()" `shouldParseAt` sm 1 31 (send 2 7 (tag 2 1) (name 5 "c") Nothing (var 7 "x") (tau 9 14 (tag 9 2) (disc 26 31 []))) (tau 18 23 (tag 18 3) (disc 26 31 []))
            parseP `shouldFailOn` "(t1:tau + t2:tau)"
        it "can parse sugared if-then-else with a shared continuation" $ do
            parseP "(if true then t1:tau else t2:tau).disc()" `shouldParseAt` ite 1 40 (bool 5 True) (tau 15 20 (tag 15 1) (disc 35 40 [])) (tau 27 32 (tag 27 2) (disc 35 40 []))
            parseP "(if true then t1:tau.t2:c!x else t3:tau).disc()" `shouldParseAt` ite 1 47 (bool 5 True) (tau 15 20 (tag 15 1) (send 22 27 (tag 22 2) (name 25 "c") Nothing (var 27 "x") (disc 42 47 []))) (tau 34 39 (tag 34 3) (disc 42 47 []))
            parseP `shouldFailOn` "(if true then t1:tau else t2:tau)"
        it "excludes trailing whitespace from spans" $ do
            parseP "t1:X(qb1) . disc()" `shouldParseAt` apply 1 9 (tag 1 1) (name 4 "X") [qb 6 1] (disc 13 18 [])
            parseP "(if true then disc() else disc() )" `shouldParseAt` ite 2 32 (bool 5 True) (disc 15 20 []) (disc 27 32 [])
        it "includes a parenthesised send value in the prefix span" $
            parseP "t1:c!(x).disc()" `shouldParseAt` send 1 8 (tag 1 1) (name 4 "c") Nothing (var 7 "x") (disc 10 15 [])
        it "regressions" $ do
            parseP "t1:M01(qb0>x).(disc(qb0)||t2:c!x.disc())" `shouldParseAt` meas 1 13 (tag 1 1) (name 4 "M01") [qb 8 0] (name 12 "x") (par 16 39 (disc 16 24 [qb 21 0]) (send 27 32 (tag 27 2) (name 30 "c") Nothing (var 32 "x") (disc 34 39 [])))

parseD :: Text -> Either (ParseErrorBundle Text Void) RawDeclarationFC
parseD = runQuarcParser (pDeclaration <* eof) fname

declarationSpec :: Spec
declarationSpec =
    describe "declaration parser" $ do
        it "accepts new proc syntax with qubit brackets" $ do
            parseD "proc A[q] := disc(q)" `shouldSatisfy` (\case Right _ -> True; _ -> False)
            parseD "proc A[q, r](c: chan bit) := disc(q)" `shouldSatisfy` (\case Right _ -> True; _ -> False)
            parseD "proc A(c: chan bit, x: bit) := disc()" `shouldSatisfy` (\case Right _ -> True; _ -> False)
            parseD "proc A := disc()" `shouldSatisfy` (\case Right _ -> True; _ -> False)
        it "spans a declaration up to the end of its body" $
            parseD "proc A := disc() " `shouldParseAt` wd 1 16 (RawDecl (name 6 "A") [] [] [] [] (disc 11 16 []))
        it "rejects qubit parameters in parentheses" $ do
            parseD `shouldFailOn` "proc A(q: qubit) := disc(q)"
            parseD `shouldFailOn` "proc A(q: qubit, c: chan bit) := disc(q)"
        it "rejects channel parameters after value parameters" $ do
            parseD `shouldFailOn` "proc A(x: bit, c: chan bit) := disc()"
            parseD `shouldFailOn` "proc A(x: bit, c: chan bit, y: bool) := disc()"

genExprText :: Bool -> Gen String
genExprText = Gen.recursive Gen.choice <$> leaves <*> compounds
  where
    paren x = "(" ++ x ++ ")"
    bin op a b = paren (a ++ " " ++ op ++ " " ++ b)
    leaves isBool
        | isBool = [Gen.element ["true", "false"]]
        | otherwise = [show <$> Gen.int (Range.linear 0 20), ("0b" ++) <$> Gen.list (Range.linear 1 4) (Gen.element ['0', '1'])]
    compounds isBool
        | isBool =
            [ Gen.subterm2 (genExprText True) (genExprText True) (bin "/\\")
            , Gen.subterm2 (genExprText True) (genExprText True) (bin "\\/")
            , Gen.subterm (genExprText True) (paren . ("!" ++))
            , Gen.element ["=", "<=", "<"] >>= \op -> Gen.subterm2 (genExprText False) (genExprText False) (bin op)
            ]
        | otherwise =
            [ Gen.element ["+", "-", "*", "&", "|", "^"] >>= \op -> Gen.subterm2 (genExprText False) (genExprText False) (bin op)
            , Gen.subterm (genExprText False) (paren . ("-" ++))
            ]

renameTags :: (Integer -> Integer) -> ProcessFC -> ProcessFC
renameTags f (WD m p) = WD m $ case p of
    Tau t k -> Tau (f <$> t) (go k)
    Tau2 t1 t2 k -> Tau2 (f <$> t1) (f <$> t2) (go k)
    Send t ch e k -> Send (f <$> t) ch e (go k)
    Recv t ch v k -> Recv (f <$> t) ch v (go k)
    Apply t op qs k -> Apply (f <$> t) op qs (go k)
    Meas t ms qs v k -> Meas (f <$> t) ms qs v (go k)
    ITE e l r -> ITE e (go l) (go r)
    Sum ps -> Sum (go <$> ps)
    Par ps -> Par (go <$> ps)
    Restr k ch -> Restr (go k) ch
    other -> other
  where
    go = renameTags f

tagsIn :: ProcessFC -> [Integer]
tagsIn (WD _ p) = case p of
    Tau t k -> val t : tagsIn k
    Tau2 t1 t2 k -> val t1 : val t2 : tagsIn k
    Send t _ _ k -> val t : tagsIn k
    Recv t _ _ k -> val t : tagsIn k
    Apply t _ _ k -> val t : tagsIn k
    Meas t _ _ _ k -> val t : tagsIn k
    ITE _ l r -> tagsIn l ++ tagsIn r
    Sum ps -> concatMap tagsIn ps
    Par ps -> concatMap tagsIn ps
    Restr k _ -> tagsIn k
    _ -> []

sortOperands :: ProcessFC -> ProcessFC
sortOperands (WD m p) = WD m $ case p of
    Sum ps -> Sum (sortOn show (sortOperands <$> ps))
    Par ps -> Par (sortOn show (sortOperands <$> ps))
    Tau t k -> Tau t (sortOperands k)
    Tau2 t1 t2 k -> Tau2 t1 t2 (sortOperands k)
    Send t ch e k -> Send t ch e (sortOperands k)
    Recv t ch v k -> Recv t ch v (sortOperands k)
    Apply t op qs k -> Apply t op qs (sortOperands k)
    Meas t ms qs v k -> Meas t ms qs v (sortOperands k)
    ITE e l r -> ITE e (sortOperands l) (sortOperands r)
    Restr k ch -> Restr (sortOperands k) ch
    other -> other

shape :: ProcessFC -> String
shape = unsolved . show . sortOperands

autoTagCount :: ProcessFC -> Int
autoTagCount = length . nub . filter (< 0) . tagsIn

unsolved :: String -> String
unsolved ('T' : 'V' : 'a' : 'r' : ' ' : rest) = "TVar _" ++ unsolved (dropWhile (`elem` ['0' .. '9']) rest)
unsolved (c : rest) = c : unsolved rest
unsolved [] = []

inferText :: (RawProcessFC -> RawProcessFC) -> String -> PropertyT IO ProcessFC
inferText prepare src = do
    raw <- evalEither (runQuarcParser (pProcess <* eof) fname (T.pack src))
    evalEither (infer opsMap [] (prepare raw))

inferExprText :: String -> PropertyT IO (ExprFC, Types)
inferExprText src = evalEither . inferExpr opsMap =<< evalEither (parseE (T.pack src))

branch :: RawProcessFC -> RawProcessFC
branch = fst . retagProcess []

roundTripSpec :: Spec
roundTripSpec =
    describe "ASCII printer" $ do
        it "prints expressions that parse back to themselves" $ checkProp $ property $ do
            src <- forAll (Gen.bool >>= genExprText)
            (e, _) <- inferExprText src
            let printed = asciiExpr e
            annotate printed
            (e', _) <- inferExprText printed
            e' === e
        it "omits automatic tags, which tagging the output gives back" $ checkProp $ property $ do
            (_, src) <- forAll genProcess
            p <- inferText branch (T.unpack src)
            let printed = asciiProcess p
            annotate printed
            p' <- inferText branch printed
            let erased = renameTags (`max` 0)
            shape (erased p') === shape (erased p)
            autoTagCount p' === autoTagCount p
        it "prints automatic tags in positive form, which parses back as explicit tags" $ checkProp $ property $ do
            (_, src) <- forAll genProcess
            p <- inferText branch (T.unpack src)
            let printed = asciiProcessWith PositiveAutoTags p
            annotate printed
            p' <- inferText id printed
            shape p' === shape (renameTags abs p)
        it "prints declarations that parse back to themselves" $
            mapM_
                ( \src -> do
                    let checked txt = case inferDeclarations opsMap . pure =<< either (error . show) Right (parseD txt) of
                            Right (_, [(declName, DeclOk decl _)]) -> (declName, decl)
                            _ -> error ("does not typecheck: " ++ T.unpack txt)
                        (n, d) = checked src
                        printed = asciiDecl n d
                    (printed, unsolved (show (snd (checked (T.pack printed))))) `shouldBe` (printed, unsolved (show d))
                )
                [ "proc A[q] := t1:X(q).disc(q)"
                , "proc B[q, r](c: chan bit, x: bit) := t1:c!x.disc(q, r)"
                , "proc C(c: chan qubit) := t1:c?y.disc(y)"
                , "proc D(x: bits 3) := if x = 0 then t1:tau.disc() else t2:tau.disc()"
                , "proc E with tag_offset(b) := $b+1:tau.disc()"
                , "proc F[q] := X(q).tau.disc(q)"
                ]
