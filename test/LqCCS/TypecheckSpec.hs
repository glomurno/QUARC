{-# LANGUAGE OverloadedStrings #-}

module LqCCS.TypecheckSpec (spec) where

import Common
import Hedgehog (annotateShow, forAll, property, (===))
import Test.Hspec

import LqCCS.Quantum
import LqCCS.Typecheck
import LqCCS.Types

spec :: Spec
spec = do
    exprSpec
    processSpec
    declarationSpec

shouldCheck :: forall a. (HasCallStack, Show a, Eq a) => Either TypecheckError a -> a -> Expectation
shouldCheck k ret = k `shouldBe` Right ret

shouldFail :: forall a. (HasCallStack, Show a, Eq a) => Either TypecheckError a -> TypecheckError -> Expectation
shouldFail k err = k `shouldBe` Left err

exprSpec :: Spec
exprSpec =
    describe "expression typechecker" $ do
        it "can typecheck boolean literals" $ do
            typecheckE "true" `shouldCheck` (wd (BoolLit True), TBool)
            typecheckE "false" `shouldCheck` (wd (BoolLit False), TBool)
        it "can typecheck integer literals" $ do
            typecheckE "1" `shouldCheck` (wd (IntLit 1), TBitsAny)
            typecheckE "42" `shouldCheck` (wd (IntLit 42), TBitsAny)
            typecheckE "-42" `shouldCheck` (wd (Neg (wd (IntLit 42))), TBitsAny)
        it "can typecheck qubit literals" $ do
            typecheckE "qb0" `shouldCheck` (wd (QubitLit 0), TQuant)
            typecheckE "qb3" `shouldCheck` (wd (QubitLit 3), TQuant)
        it "can typecheck variables" $ do
            typecheckE "x" `shouldCheck` (wd (Ref (Var "x" (TVar 0))), TVar 0)
        it "can typecheck boolean operators" $ do
            typecheckE "1 = 1" `shouldCheck` (wd (Eq_ (wd (IntLit 1)) (wd (IntLit 1))), TBool)
            typecheckE "2 = qb0" `shouldFail` TypeMismatch (fc 1 7) TBitsAny TQuant
            typecheckE "true = false" `shouldCheck` (wd (Eq_ (wd (BoolLit True)) (wd (BoolLit False))), TBool)
            typecheckE "true = x" `shouldCheck` (wd (Eq_ (wd (BoolLit True)) (wd (Ref (Var "x" TBool)))), TBool)
            typecheckE "!true" `shouldCheck` (wd (Not (wd (BoolLit True))), TBool)
            typecheckE "!2" `shouldFail` TypeMismatch (fc 2 2) TBitsAny TBool
            typecheckE "true /\\ false" `shouldCheck` (wd (And (wd (BoolLit True)) (wd (BoolLit False))), TBool)
            typecheckE "z /\\ false" `shouldCheck` (wd (And (wd (Ref (Var "z" TBool))) (wd (BoolLit False))), TBool)
            typecheckE "true /\\ 1" `shouldFail` TypeMismatch (fc 9 9) TBitsAny TBool
            typecheckE "1 /\\ 2" `shouldFail` TypeMismatch (fc 1 1) TBitsAny TBool
            typecheckE "1 <= 1" `shouldCheck` (wd (LEq (wd (IntLit 1)) (wd (IntLit 1))), TBool)
            typecheckE "1 <= x" `shouldCheck` (wd (LEq (wd (IntLit 1)) (wd (Ref (Var "x" TBitsAny)))), TBool)
            typecheckE "2 <= qb0" `shouldFail` TypeMismatch (fc 1 1) TBitsAny TQuant
            typecheckE "true <= false" `shouldFail` TypeMismatch (fc 1 4) TBool TBitsAny
        it "can typecheck integer operators" $ do
            typecheckE "1 + 1" `shouldCheck` (wd (Plus (wd (IntLit 1)) (wd (IntLit 1))), TBitsAny)
            typecheckE "x + 1" `shouldCheck` (wd (Plus (wd (Ref (Var "x" TBitsAny))) (wd (IntLit 1))), TBitsAny)
            typecheckE "2 + qb0" `shouldFail` TypeMismatch (fc 1 1) TBitsAny TQuant
            typecheckE "z + qb0" `shouldFail` TypeMismatch (fc 1 1) TBitsAny TQuant
            typecheckE "true + false" `shouldFail` TypeMismatch (fc 1 4) TBool TBitsAny
            typecheckE "1 * 1" `shouldCheck` (wd (Mul (wd (IntLit 1)) (wd (IntLit 1))), TBitsAny)
            typecheckE "2 * qb0" `shouldFail` TypeMismatch (fc 1 1) TBitsAny TQuant
            typecheckE "-1" `shouldCheck` (wd (Neg (wd (IntLit 1))), TBitsAny)
            typecheckE "-true" `shouldFail` TypeMismatch (fc 2 5) TBool TBitsAny
        it "can typecheck complex variable usage" $ do
            typecheckE "x + y" `shouldCheck` (wd (Plus (wd (Ref (Var "x" TBitsAny))) (wd (Ref (Var "y" TBitsAny)))), TBitsAny)
            typecheckE "true /\\ (x = 0)" `shouldCheck` (wd (And (wd (BoolLit True)) (wd (Eq_ (wd (Ref (Var "x" TBitsAny))) (wd (IntLit 0))))), TBool)
            typecheckE "x /\\ (x = 0)" `shouldFail` TypeMismatch (fc 7 11) TBool TBitsAny
            typecheckE "x + (x * 0)" `shouldCheck` (wd (Plus (wd (Ref (Var "x" TBitsAny))) (wd (Mul (wd (Ref (Var "x" TBitsAny))) (wd (IntLit 0))))), TBitsAny)

processSpec :: Spec
processSpec =
    describe "process typechecker" $ do
        it "can typecheck discards" $ do
            typecheckP "disc()" `shouldCheck` wd (Discard [])
            typecheckP "disc(x)" `shouldCheck` wd (Discard [wd (QVar "x")])
            typecheckP "disc(x,y)" `shouldCheck` wd (Discard [wd (QVar "x"), wd (QVar "y")])
            typecheckP "disc(qb1)" `shouldCheck` wd (Discard [wd (QConst 1)])
            typecheckP "disc(qb1,x)" `shouldCheck` wd (Discard [wd (QConst 1), wd (QVar "x")])
            typecheckP "disc(x,x)" `shouldFail` NonUniqueArgument (fc 6 6) (fc 8 8)
            typecheckP "disc(x,qb0,x)" `shouldFail` NonUniqueArgument (fc 6 6) (fc 12 12)
            typecheckP "disc(qb0,qb0)" `shouldFail` NonUniqueArgument (fc 6 8) (fc 10 12)
            typecheckP "disc(1)" `shouldFail` TypeMismatch (fc 6 6) TBitsAny TQuant
            typecheckP "disc(true)" `shouldFail` TypeMismatch (fc 6 9) TBool TQuant
        it "can typecheck tau" $ do
            typecheckP "t1:tau.disc()" `shouldCheck` wd (Tau (wd 1) (wd (Discard [])))
            typecheckP "t1:tau.t2:tau.disc()" `shouldCheck` wd (Tau (wd 1) (wd (Tau (wd 2) (wd (Discard [])))))
            typecheckP "t1:tau.disc(x,x)" `shouldFail` NonUniqueArgument (fc 13 13) (fc 15 15)
            typecheckP "t1,t2:tau.disc()" `shouldCheck` wd (Tau2 (wd 1) (wd 2) (wd (Discard [])))
            typecheckP "t1,t2:tau.t3:tau.disc()" `shouldCheck` wd (Tau2 (wd 1) (wd 2) (wd (Tau (wd 3) (wd (Discard [])))))
        it "can typecheck send" $ do
            typecheckP "t1:c!2.disc()" `shouldCheck` wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 2)) (wd (Discard [])))
            typecheckP "t1:c:bit!2.disc()" `shouldCheck` wd (Send (wd 1) (wd (Channel "c" (TBits 1))) (wd (IntLit 2)) (wd (Discard [])))
            typecheckP "t1:c:bool!2.disc()" `shouldFail` TypeMismatch (fc 4 4) TBool TBitsAny
            typecheckP "t1:foo!1<=1.disc()" `shouldCheck` wd (Send (wd 1) (wd (Channel "foo" TBool)) (wd (LEq (wd (IntLit 1)) (wd (IntLit 1)))) (wd (Discard [])))
            typecheckP "t1:d!qb0.disc()" `shouldCheck` wd (Send (wd 1) (wd (Channel "d" TQuant)) (wd (QubitLit 0)) (wd (Discard [])))
            typecheckP "t1:d!qb0.disc(qb0)" `shouldFail` NonLinearUsage (fc 6 8)
            typecheckP "t1:d!qb0.disc(qb1)" `shouldCheck` wd (Send (wd 1) (wd (Channel "d" TQuant)) (wd (QubitLit 0)) (wd (Discard [wd (QConst 1)])))
            typecheckP "t1:c!x.disc()" `shouldCheck` wd (Send (wd 1) (wd (Channel "c" (TVar 0))) (wd (Ref (Var "x" (TVar 0)))) (wd (Discard [])))
            typecheckP "t1:c!x.disc(x)" `shouldFail` NonLinearUsage (fc 6 6)
            typecheckP "t1:c:qubit!x.disc()" `shouldCheck` wd (Send (wd 1) (wd (Channel "c" TQuant)) (wd (Ref (Var "x" TQuant))) (wd (Discard [])))
        it "can typecheck receive" $ do
            typecheckP "t1:c?x.disc()" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" (TVar 1))) (wd (Var "x" (TVar 1))) (wd (Discard [])))
            typecheckP "t1:c:bit?x.disc()" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" (TBits 1))) (wd (Var "x" (TBits 1))) (wd (Discard [])))
            typecheckP "t1:c?x.disc(x)" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" TQuant)) (wd (Var "x" TQuant)) (wd (Discard [wd (QVar "x")])))
            typecheckP "t1:c?x.t2:c!x+1.disc()" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" TBitsAny)) (wd (Var "x" TBitsAny)) (wd (Send (wd 2) (wd (Channel "c" TBitsAny)) (wd (Plus (wd (Ref (Var "x" TBitsAny))) (wd (IntLit 1)))) (wd (Discard [])))))
            typecheckP "t1:c:bit?x.t2:c:bool?x.disc()" `shouldFail` TypeMismatch (fc 6 8) TBool (TBits 1)
            typecheckP "t1:c:bit?x.t2:c:bit?y.disc()" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" (TBits 1))) (wd (Var "x" (TBits 1))) (wd (Recv (wd 2) (wd (Channel "c" (TBits 1))) (wd (Var "y" (TBits 1))) (wd (Discard [])))))
            typecheckP "t1:c:bit?x.t2:d:bool?x.disc()" `shouldFail` TypeMismatch (fc 6 8) TBool (TBits 1)
        it "can typecheck apply" $ do
            typecheckP "t1:H(qb0).disc(qb0)" `shouldCheck` wd (Apply (wd 1) (wd hOp) [wd (QConst 0)] (wd (Discard [wd (QConst 0)])))
            typecheckP "t1:CNOT(x,y).disc(y,x)" `shouldCheck` wd (Apply (wd 1) (wd cnotOp) [wd (QVar "x"), wd (QVar "y")] (wd (Discard [wd (QVar "y"), wd (QVar "x")])))
            typecheckP "t1:H(qb0).t2:X(qb0).disc(qb0)" `shouldCheck` wd (Apply (wd 1) (wd hOp) [wd (QConst 0)] (wd (Apply (wd 2) (wd xOp) [wd (QConst 0)] (wd (Discard [wd (QConst 0)])))))
            typecheckP "t1:H(x).disc()" `shouldFail` NonLinearUsage (fc 6 6)
            typecheckP "t1:H(qb0).disc()" `shouldFail` NonLinearUsage (fc 6 8)
            typecheckP "t1:CNOT(x,x).disc(x)" `shouldFail` NonUniqueArgument (fc 9 9) (fc 11 11)
            typecheckP "t1:c:bit?x.t2:H(x).disc(x)" `shouldFail` TypeMismatch (fc 6 8) TQuant (TBits 1)
            typecheckP "t1:H(qb0,qb1).disc()" `shouldFail` WrongNumberOfArguments (fc 6 12) 2 1
        it "can typecheck measure" $ do
            typecheckP "t1:MRandBit(>x).disc()" `shouldCheck` wd (Meas (wd 1) (wd bitMeas) [] (wd (Var "x" (TBits 1))) (wd (Discard [])))
            typecheckP "t1:M01(qb0>x).disc(qb0)" `shouldCheck` wd (Meas (wd 1) (wd stdMeas) [wd (QConst 0)] (wd (Var "x" (TBits 1))) (wd (Discard [wd (QConst 0)])))
            typecheckP "t1:MBell(x,y>z).disc(y,x)" `shouldCheck` wd (Meas (wd 1) (wd bellMeas) [wd (QVar "x"), wd (QVar "y")] (wd (Var "z" (TBits 2))) (wd (Discard [wd (QVar "y"), wd (QVar "x")])))
            typecheckP "t1:M01(qb0>x).t2:MH(qb1>y).disc(qb1,qb0)" `shouldCheck` wd (Meas (wd 1) (wd stdMeas) [wd (QConst 0)] (wd (Var "x" (TBits 1))) (wd (Meas (wd 2) (wd hadMeas) [wd (QConst 1)] (wd (Var "y" (TBits 1))) (wd (Discard [wd (QConst 1), wd (QConst 0)])))))
            typecheckP "t1:MBell(qb0,qb0>x).disc(qb0)" `shouldFail` NonUniqueArgument (fc 10 12) (fc 14 16)
            typecheckP "t1:M01(x>x).disc(x)" `shouldFail` TypeMismatch (fc 10 10) TQuant (TBits 1)
            typecheckP "t1:M01(qb0>x).disc()" `shouldFail` NonLinearUsage (fc 8 10)
            typecheckP "t1:M01(y>x).disc()" `shouldFail` NonLinearUsage (fc 8 8)
            typecheckP "t1:M01(y,z>x).disc()" `shouldFail` WrongNumberOfArguments (fc 8 10) 2 1
            typecheckP "t1:c:bit?x.t2:M01(x>y).disc(x)" `shouldFail` TypeMismatch (fc 6 8) TQuant (TBits 1)
        it "can typecheck conditionals" $ do
            typecheckP "if true then disc() else disc()" `shouldCheck` wd (ITE (wd (BoolLit True)) (wd (Discard [])) (wd (Discard [])))
            typecheckP "if true then disc(x) else disc()" `shouldFail` WrongBranchLinearity (fc 14 20) (fc 27 32) [fc 19 19]
            typecheckP "if true then disc() else disc(x)" `shouldFail` WrongBranchLinearity (fc 14 19) (fc 26 32) [fc 31 31]
            typecheckP "if true then disc(x) else disc(x)" `shouldCheck` wd (ITE (wd (BoolLit True)) (wd (Discard [wd (QVar "x")])) (wd (Discard [wd (QVar "x")])))
            typecheckP "if false then t1:c!1.disc() else disc()" `shouldCheck` wd (ITE (wd (BoolLit False)) (wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 1)) (wd (Discard [])))) (wd (Discard [])))
        it "can typecheck non-deterministic sum" $ do
            typecheckP "disc() + disc()" `shouldCheck` wd (Sum [wd (Discard []), wd (Discard [])])
            typecheckP "disc(x) + disc()" `shouldFail` WrongBranchLinearity (fc 1 7) (fc 11 16) [fc 6 6]
            typecheckP "disc() + disc(x)" `shouldFail` WrongBranchLinearity (fc 1 6) (fc 10 16) [fc 15 15]
            typecheckP "disc(qb0) + disc(qb0)" `shouldCheck` wd (Sum [wd (Discard [wd (QConst 0)]), wd (Discard [wd (QConst 0)])])
            typecheckP "(t1:c!1.disc()) + disc()" `shouldCheck` wd (Sum [wd (Discard []), wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 1)) (wd (Discard [])))])
        it "can typecheck parallel" $ do
            typecheckP "disc() || disc()" `shouldCheck` wd (Par [wd (Discard []), wd (Discard [])])
            typecheckP "disc(x) || disc()" `shouldCheck` wd (Par [wd (Discard []), wd (Discard [wd (QVar "x")])])
            typecheckP "disc() || disc(x)" `shouldCheck` wd (Par [wd (Discard []), wd (Discard [wd (QVar "x")])])
            typecheckP "disc(x) || disc(x)" `shouldFail` WrongBranchLinearity (fc 1 7) (fc 12 18) [fc 6 6]
            typecheckP "(t1:c!1.disc()) || disc(x)" `shouldCheck` wd (Par [wd (Discard [wd (QVar "x")]), wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 1)) (wd (Discard [])))])
        it "can typecheck restriction" $ do
            typecheckP "(t1:c!1.disc()) \\ c" `shouldCheck` wd (Restr (wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 1)) (wd (Discard [])))) (wd (Channel "c" TBitsAny)))
            typecheckP "disc() \\ c" `shouldCheck` wd (Restr (wd (Discard [])) (wd (Channel "c" (TVar 0))))
        it "regressions" $ do
            typecheckP "t1:M01(qb0>x).(disc(qb0)||t2:c!x.disc())" `shouldCheck` wd (Meas (wd 1) (wd stdMeas) [wd (QConst 0)] (wd (Var "x" (TBits 1))) (wd (Par [wd (Discard [wd (QConst 0)]), wd (Send (wd 2) (wd (Channel "c" (TBits 1))) (wd (Ref (Var "x" (TBits 1)))) (wd (Discard [])))])))

declarationSpec :: Spec
declarationSpec = do
    describe "declaration typechecker" $ do
        it "can typecheck process with no declarations" $ do
            typecheckD [] "disc()" `shouldCheck` wd (Discard [])
            typecheckD [] "if false then t1:c!1.disc() else disc()" `shouldCheck` wd (ITE (wd (BoolLit False)) (wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 1)) (wd (Discard [])))) (wd (Discard [])))
            typecheckD [] "disc(x) + disc()" `shouldFail` WrongBranchLinearity (fc 1 7) (fc 11 16) [fc 6 6]
        it "can typecheck process as declarations" $ do
            typecheckD ["proc A[x] := disc(x)"] "@A[x]" `shouldCheck` wd (Call (wd "A") [wd (QVar "x")] [] [])
            typecheckD ["proc A[x] := disc(x)"] "@A" `shouldFail` WrongNumberOfArguments NoFC 0 1
            typecheckD ["proc A := disc(x)"] "@A[x]" `shouldFail` FreeVarInDeclaration (fc 16 16) "x"
            typecheckD ["proc A := disc(qb0)"] "@A" `shouldFail` QConstInDeclaration (fc 16 18) 0
            typecheckD ["proc A := t1:M01(x>x).disc(x)"] "@A" `shouldFail` TypeMismatch (fc 20 20) TQuant (TBits 1)
            typecheckD ["proc A(x:bit) := t1:c?x.t2:c!x+1.disc()"] "@A(x)" `shouldCheck` wd (Call (wd "A") [] [] [wd (Ref $ Var "x" (TBits 1))])
        it "can typecheck disjoint declarations" $ do
            typecheckD ["proc A[x] := disc(x)", "proc B := disc()"] "@A[x] || @B" `shouldCheck` wd (Par [wd (Call (wd "A") [wd (QVar "x")] [] []), wd (Call (wd "B") [] [] [])])
            typecheckD ["proc A := disc()", "proc B[x] := disc(x)"] "@A || @B[x]" `shouldCheck` wd (Par [wd (Call (wd "A") [] [] []), wd (Call (wd "B") [wd (QVar "x")] [] [])])
            typecheckD ["proc A[y] := disc(y)", "proc B[x] := disc(x)"] "@A[y] || @B[x]" `shouldCheck` wd (Par [wd (Call (wd "A") [wd (QVar "y")] [] []), wd (Call (wd "B") [wd (QVar "x")] [] [])])
            typecheckD ["proc A[x] := disc(x)", "proc B[x] := disc(x)"] "@A[x] || @B[x]" `shouldFail` WrongBranchLinearity (fc 1 2) (fc 10 11) [fc 19 19]
            typecheckD ["proc A := disc()", "proc B[x] := disc(x)"] "@A + @B[x]" `shouldFail` WrongBranchLinearity (fc 1 2) (fc 6 7) [fc 19 19]
            typecheckD ["proc A[y] := disc(y)", "proc B[x] := disc(x)"] "@A[x] + @B[x]" `shouldCheck` wd (Sum [wd (Call (wd "A") [wd (QVar "x")] [] []), wd (Call (wd "B") [wd (QVar "x")] [] [])])
            typecheckD ["proc A := disc()"] "t1:c!0.@A" `shouldCheck` wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 0)) (wd (Call (wd "A") [] [] [])))
            typecheckD ["proc A := disc()"] "(t1:c!0.@A) + t2:d!0.@A" `shouldCheck` wd (Sum [wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 0)) (wd (Call (wd "A") [] [] []))), wd (Send (wd 2) (wd (Channel "d" TBitsAny)) (wd (IntLit 0)) (wd (Call (wd "A") [] [] [])))])
            typecheckD ["proc A[x] := disc(x)"] "(t1:c!0.@A[qb0]) + t2:d!0.@A[qb0]" `shouldCheck` wd (Sum [wd (Send (wd 1) (wd (Channel "c" TBitsAny)) (wd (IntLit 0)) (wd (Call (wd "A") [wd (QConst 0)] [] []))), wd (Send (wd 2) (wd (Channel "d" TBitsAny)) (wd (IntLit 0)) (wd (Call (wd "A") [wd (QConst 0)] [] [])))])
        it "can typecheck bound declarations" $ do
            typecheckD ["proc A[x] := disc(x)"] "t1:H(qb0).@A[qb0]" `shouldCheck` wd (Apply (wd 1) (wd hOp) [wd (QConst 0)] (wd (Call (wd "A") [wd (QConst 0)] [] [])))
            typecheckD ["proc A[x] := disc(x)"] "t1:H(qb0).@A[qb1]" `shouldFail` NonLinearUsage (fc 6 8)
            typecheckD ["proc A(x:bit) := t2:d!x+1.disc()"] "t1:c?x.@A(x)" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" (TBits 1))) (wd (Var "x" (TBits 1))) (wd (Call (wd "A") [] [] [wd (Ref $ Var "x" (TBits 1))])))
            typecheckD ["proc A(x:bool) := t2:d!x \\/ false.disc()"] "t1:c:bit?x.@A(x)" `shouldFail` TypeMismatch (fc 6 8) TBool (TBits 1)
            typecheckD ["proc A(x:bit) := t2:d!x.disc()"] "@A(x)" `shouldCheck` wd (Call (wd "A") [] [] [wd (Ref $ Var "x" (TBits 1))])
            typecheckD ["proc A(x:bit) := t2:d!x.disc()"] "t1:c:bit?x.@A(x)" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" (TBits 1))) (wd (Var "x" (TBits 1))) (wd (Call (wd "A") [] [] [wd (Ref $ Var "x" (TBits 1))])))
            typecheckD ["proc A(x:bool) := t2:d!x.disc()"] "t1:c:bool?x.@A(x)" `shouldCheck` wd (Recv (wd 1) (wd (Channel "c" TBool)) (wd (Var "x" TBool)) (wd (Call (wd "A") [] [] [wd (Ref $ Var "x" TBool)])))
            typecheckD ["proc A(x:bool) := t2:d!x.disc()"] "(t1:c:bool?x.@A(x)) + t1:c:bit?x.@A(x)" `shouldFail` TypeMismatch (fc 28 30) TBool (TBits 1)
        it "can typecheck channel parameter declarations" $ do
            typecheckD ["proc A(c: chan bit) := t1:c?x.t2:c!x+1.disc()"] "@A(c)" `shouldCheck` wd (Call (wd "A") [] [wd (Channel "c" (TBits 1))] [])
            typecheckD ["proc A(c: chan bit, x: bit) := t1:c!x.disc()"] "@A(c, 42)" `shouldCheck` wd (Call (wd "A") [] [wd (Channel "c" (TBits 1))] [wd (IntLit 42)])
            typecheckD ["proc A[q](c: chan bit) := t1:c!0.disc(q)"] "@A[qb0](c)" `shouldCheck` wd (Call (wd "A") [wd (QConst 0)] [wd (Channel "c" (TBits 1))] [])
            typecheckD ["proc A(c: chan bit) := t1:c?x.t2:c!x+1.disc()"] "@A" `shouldFail` WrongNumberOfArguments NoFC 0 1
        it "rejects a non-channel argument for a channel parameter" $ do
            typecheckD ["proc A(c: chan bit) := t1:c!0.disc()"] "@A(0)" `shouldFail` ExpectedChannelArgument (fc 4 4) "A" "c"
            typecheckD ["proc A(d: chan bit, c: chan bit) := t1:c!0.disc()"] "@A(d, 1 + 1)" `shouldFail` ExpectedChannelArgument (fc 7 11) "A" "c"
        it "rejects a channel parameter used as a value" $ do
            typecheckD ["proc A(c: chan bit) := if c = 0 then disc() else disc()"] "@A(d)" `shouldFail` ChannelAsValue (fc 27 27) "c"
            typecheckD ["proc A(c: chan bit, d: chan bit) := t1:d!c.disc()"] "@A(c, d)" `shouldFail` ChannelAsValue (fc 42 42) "c"
        it "allows a received variable named like a channel parameter" $ do
            typecheckD ["proc A(c: chan bit, d: chan bit) := t1:d?c.t2:d!c.disc()"] "@A(e, f)" `shouldSatisfy` either (const False) (const True)
        it "can detect recursion" $ do
            typecheckD ["proc A := @A"] "@A" `shouldFail` RecursiveDeclaration (fc 11 12) "A"
            typecheckD ["proc A := t1:tau.@A"] "@A" `shouldFail` RecursiveDeclaration (fc 18 19) "A"
            typecheckD ["proc B := @A", "proc A := @B"] "@A" `shouldFail` RecursiveDeclaration (fc 11 12) "B"
            typecheckD ["proc B := @A", "proc A := @B"] "@B" `shouldFail` RecursiveDeclaration (fc 11 12) "B"
    describe "per-declaration typechecker" $ do
        let statuses = fmap (fmap (fmap status) . snd) . inferDeclarations opsMap . fmap parseD
            status DeclOk{} = "ok"
            status (DeclFailed _) = "failed"
            status (DeclSkipped deps) = "skipped " ++ unwords deps
        it "checks each declaration independently" $ do
            statuses ["proc A[x] := disc(x)", "proc B := disc(qb0)", "proc C[x] := disc(x)"]
                `shouldBe` Right [("A", "ok"), ("B", "failed"), ("C", "ok")]
        it "skips declarations calling a failed one" $ do
            statuses ["proc A := disc(qb0)", "proc B := @A", "proc C := @B", "proc D[x] := disc(x)"]
                `shouldBe` Right [("A", "failed"), ("B", "skipped A"), ("C", "skipped B"), ("D", "ok")]
        it "fails as a whole on recursion" $ do
            statuses ["proc B := @A", "proc A := @B"] `shouldBe` Left (RecursiveDeclaration (fc 11 12) "B")
        it "typechecks a process against the checked declarations" $ do
            ctxt <- either (\e -> expectationFailure (show e) >> undefined) (pure . fst) $ inferDeclarations opsMap [parseD "proc A[x] := disc(x)"]
            fst (inferWith ctxt (parseP "@A[qb0]")) `shouldCheck` wd (Call (wd "A") [wd (QConst 0)] [] [])
            fst (inferWith ctxt (parseP "@B")) `shouldFail` DeclarationNotFound (fc 1 2) "B"
    describe "properties" $
        it "accepts generated closed processes" $ checkProp $ property $ do
            (_, p) <- forAll genProcess
            let res = typecheckP p
            annotateShow res
            either (const False) (const True) res === True
    describe "regressions" $ do
        it "unexpected free qubit" $ do
            typecheckD [] "(t1:c!qb0.disc()) || (t2:c?q.disc(q))" `shouldCheck` wd (Par [wd $ Send (wd 1) (wd $ Channel "c" TQuant) (wd $ QubitLit 0) (wd $ Discard []), wd $ Recv (wd 2) (wd $ Channel "c" TQuant) (wd $ Var "q" TQuant) (wd $ Discard [wd $ QVar "q"])])
            typecheckD ["proc B := t2:c?q.disc(q)"] "(t1:c!qb0.disc()) || @B" `shouldCheck` wd (Par [wd $ Send (wd 1) (wd $ Channel "c" TQuant) (wd $ QubitLit 0) (wd $ Discard []), wd $ Call (wd "B") [] [] []])
