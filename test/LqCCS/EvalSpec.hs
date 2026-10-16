{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.EvalSpec (spec) where

import Data.Text (Text)

import Common
import Test.Hspec

import LqCCS.EvalTagged
import LqCCS.QDist
import LqCCS.Quantum
import LqCCS.Types

parseEvalE :: Text -> ExprFC
parseEvalE = evalExpr . fst . parseInferE

eqQState :: QState -> QState -> Bool
eqQState (QState dim1 rho1) (QState dim2 rho2) = dim1 == dim2 && approxEqDensityOp defaultApproxEpsilon rho1 rho2

eqQDist :: QDist (Process meta) -> QDist (Process meta) -> Bool
eqQDist (QDist qd1) (QDist qd2) | length qd1 == length qd2 = and $ zipWith (\s1 s2 -> eqQState (qstate s1) (qstate s2) && s1.element == s2.element) qd1 qd2
eqQDist _ _ = False

findAndRemove :: (Action meta, QDist (Process meta)) -> [(Action meta, QDist (Process meta))] -> Maybe [(Action meta, QDist (Process meta))]
findAndRemove _ [] = Nothing
findAndRemove move (y : ys)
    | fst move == fst y && eqQDist (snd move) (snd y) = Just ys
    | otherwise = (y :) <$> findAndRemove move ys

isMultisetMatch :: [(Action meta, QDist (Process meta))] -> [(Action meta, QDist (Process meta))] -> Bool
isMultisetMatch [] [] = True
isMultisetMatch (move : xs) ys = case findAndRemove move ys of
    Just ys' -> isMultisetMatch xs ys'
    Nothing -> False
isMultisetMatch _ _ = False

shouldSubStepTo :: (HasCallStack) => SubState (Process meta) -> [(Action meta, QDist (Process meta))] -> Expectation
shouldSubStepTo p expected =
    shouldSatisfy
        (subStep mempty p)
        ( \case
            Right res | length res == length expected -> isMultisetMatch res expected
            _ -> False
        )

shouldStepTo :: (HasCallStack) => QDist (Process meta) -> [(Action meta, QDist (Process meta))] -> Expectation
shouldStepTo p expected =
    shouldSatisfy
        (step mempty p)
        ( \case
            Right res | length res == length expected -> isMultisetMatch res expected
            _ -> False
        )

spec :: Spec
spec = do
    describe "expression evaluation" $ do
        it "can reduce" $ do
            parseEvalE "42" `shouldBe` wd (IntLit 42)
            parseEvalE "true" `shouldBe` wd (BoolLit True)
            parseEvalE "qb42" `shouldBe` wd (QubitLit 42)
            parseEvalE "2 + 4" `shouldBe` wd (IntLit 6)
            parseEvalE "2 = 3" `shouldBe` wd (BoolLit False)
        it "can substitute" $ do
            substExpr (Var "x" TBitsAny) (wd $ IntLit 42) (wd $ Ref (Var "x" TBitsAny)) `shouldBe` wd (IntLit 42)
            substExpr (Var "y" TBitsAny) (wd $ IntLit 42) (wd $ Ref (Var "x" TBitsAny)) `shouldBe` wd (Ref (Var "x" TBitsAny))
            substExpr (Var "x" TBitsAny) (wd $ IntLit 42) (wd $ Plus (wd $ Ref (Var "x" TBitsAny)) (wd $ IntLit 2)) `shouldBe` wd (Plus (wd $ IntLit 42) (wd $ IntLit 2))
            substExpr (Var "z" TBool) (wd $ BoolLit False) (wd $ And (wd $ BoolLit True) (wd $ Ref (Var "z" TBool))) `shouldBe` wd (And (wd $ BoolLit True) (wd $ BoolLit False))
    describe "substate stepping" $ do
        it "can substitute" $ do
            subst (Var "x" TBitsAny) (wd $ IntLit 42) (parseInferP "t2:d:qubit?y.t1:c:bits 8!x.disc(y)") `shouldBe` parseInferP "t2:d:qubit?y.t1:c:bits 8!42.disc(y)"
            subst (Var "y" TBitsAny) (wd $ QubitLit 42) (parseInferP "t2:d:qubit?y.t1:c:bits 8!x.disc(y)") `shouldBe` parseInferP "t2:d:qubit?y.t1:c:bits 8!x.disc(y)"
            subst (Var "y" TQuant) (wd $ QubitLit 42) (parseInferP "t2:d:bool?z.t1:c:bits 8!2.disc(y)") `shouldBe` parseInferP "t2:d:bool?z.t1:c:bits 8!2.disc(qb42)"
            subst (Var "x" TQuant) (wd $ QubitLit 42) (parseInferP "disc(x) + t1:tau.disc(x)") `shouldBe` parseInferP "disc(qb42) + t1:tau.disc(qb42)"
            subst (Var "x" TQuant) (wd $ QubitLit 42) (parseInferP "t1:M01(qb0>x).disc(qb0)") `shouldBe` parseInferP "t1:M01(qb0>x).disc(qb0)"
        it "detect used qubits" $ do
            usesQubit 1 (parseInferP "disc(qb1)") `shouldBe` True
            usesQubit 1 (parseInferP "disc(qb2)") `shouldBe` False
            usesQubit 1 (parseInferP "t1:c!qb1.disc()") `shouldBe` True
            usesQubit 1 (parseInferP "t1:X(qb1).disc(qb1)") `shouldBe` True
    it "can handle discard" $ do
        SubState empty (parseInferP "disc()") `shouldSubStepTo` []
        SubState zero1 (parseInferP "disc(qb1)") `shouldSubStepTo` []
    it "can handle silent actions" $ do
        SubState empty (parseInferP "t1:tau.disc()") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "t2:tau.t1:tau.disc()") `shouldSubStepTo` [(ActionTau 2, QDist [SubState empty (parseInferP "t1:tau.disc()")])]
        SubState empty (parseInferP "t1,t2:tau.disc()") `shouldSubStepTo` [(ActionTau2 1 2, QDist [SubState empty (parseInferP "disc()")])]
    it "can handle sending" $ do
        SubState empty (parseInferP "t1:c!1.disc()") `shouldSubStepTo` [(ActionSend 1 (Channel "c" TBitsAny) (wd $ IntLit 1), QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "t1:c!true.disc()") `shouldSubStepTo` [(ActionSend 1 (Channel "c" TBool) (wd $ BoolLit True), QDist [SubState empty (parseInferP "disc()")])]
        SubState zero1 (parseInferP "t3:c!qb0.t2:tau.disc()") `shouldSubStepTo` [(ActionSend 3 (Channel "c" TQuant) (wd $ QubitLit 0), QDist [SubState zero1 (parseInferP "t2:tau.disc()")])]
        SubState empty (parseInferP "t1:c!1+2.disc()") `shouldSubStepTo` [(ActionSend 1 (Channel "c" TBitsAny) (wd $ IntLit 3), QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "t1:c:bits 8!x.disc()") `shouldSubStepTo` []
    it "can handle receiving" $ do
        SubState empty (parseInferP "t1:c:bits 2?x.disc()")
            `shouldSubStepTo` [ (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "disc()")])
                              , (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "disc()")])
                              , (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 2), QDist [SubState empty (parseInferP "disc()")])
                              , (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 3), QDist [SubState empty (parseInferP "disc()")])
                              ]
        SubState empty (parseInferP "t1:c:bool?x.disc()")
            `shouldSubStepTo` [ (ActionRecvE 1 (Channel "c" TBool) (wd $ BoolLit False), QDist [SubState empty (parseInferP "disc()")])
                              , (ActionRecvE 1 (Channel "c" TBool) (wd $ BoolLit True), QDist [SubState empty (parseInferP "disc()")])
                              ]
        SubState one1 (parseInferP "t1:c:qubit?x.disc(x)") `shouldSubStepTo` [(ActionRecvE 1 (Channel "c" TQuant) (wd $ QubitLit 0), QDist [SubState one1 (parseInferP "disc(qb0)")])]
        SubState zero2 (parseInferP "t1:c:qubit?x.disc(x)") `shouldSubStepTo` [(ActionRecvE 1 (Channel "c" TQuant) (wd $ QubitLit 0), QDist [SubState zero2 (parseInferP "disc(qb0)")]), (ActionRecvE 1 (Channel "c" TQuant) (wd $ QubitLit 1), QDist [SubState zero2 (parseInferP "disc(qb1)")])]
        SubState zero2 (parseInferP "t1:c:qubit?x.disc(x,qb1)") `shouldSubStepTo` [(ActionRecvE 1 (Channel "c" TQuant) (wd $ QubitLit 0), QDist [SubState zero2 (parseInferP "disc(qb0,qb1)")])]
    it "can apply superoperators" $ do
        SubState zero1 (parseInferP "t1:X(qb0).disc(qb0)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState one1 (parseInferP "disc(qb0)")])]
        SubState zero2 (parseInferP "t1:X(qb1).disc(qb0,qb1)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState zeroone (parseInferP "disc(qb0,qb1)")])]
        SubState onezero (parseInferP "t1:CNOT(qb0,qb1).disc(qb0,qb1)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState one2 (parseInferP "disc(qb0,qb1)")])]
        SubState onezero (parseInferP "t1:CNOT(qb1,qb0).disc(qb0,qb1)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState onezero (parseInferP "disc(qb0,qb1)")])]
    it "can apply measurements" $ do
        SubState zero1 (parseInferP "t1:M01(qb0>x).t2:c!x.disc(qb0)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState zero1 (parseInferP "t2:c:bit!0.disc(qb0)")])]
        SubState one1 (parseInferP "t1:M01(qb0>x).t2:c!x.disc(qb0)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState one1 (parseInferP "t2:c:bit!1.disc(qb0)")])]
        SubState zero1 (parseInferP "t1:MH(qb0>x).t2:c!x.disc(qb0)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState (scaleQS 0.5 plus1) (parseInferP "t2:c:bit!0.disc(qb0)"), SubState (scaleQS 0.5 minus1) (parseInferP "t2:c:bit!1.disc(qb0)")])]
        SubState phiplus2 (parseInferP "t1:M201(qb0,qb1>x).t2:c!x.disc(qb0,qb1)") `shouldSubStepTo` [(ActionTau 1, QDist [SubState (scaleQS 0.5 zero2) (parseInferP "t2:c:bits 2!0.disc(qb0,qb1)"), SubState (scaleQS 0.5 one2) (parseInferP "t2:c:bits 2!3.disc(qb0,qb1)")])]
        SubState zero1 (parseInferP "t1:MRandBit(>x).t2:c!x.disc()") `shouldSubStepTo` [(ActionTau 1, QDist [SubState (scaleQS 0.5 zero1) (parseInferP "t2:c:bit!0.disc()"), SubState (scaleQS 0.5 zero1) (parseInferP "t2:c:bit!1.disc()")])]
    it "can step conditionally" $ do
        SubState empty (parseInferP "if true then t1:tau.disc() else t2:tau.disc()") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "if false then t1:tau.disc() else t2:tau.disc()") `shouldSubStepTo` [(ActionTau 2, QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "if true /\\ false then t1:tau.disc() else t2:tau.disc()") `shouldSubStepTo` [(ActionTau 2, QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "if true then disc() else t2:tau.disc()") `shouldSubStepTo` []
        SubState empty (parseInferP "if false then t1:tau.disc() else disc()") `shouldSubStepTo` []
    it "can step non-deterministically" $ do
        SubState empty (parseInferP "(t1:tau.t3:c!true.disc()) + (t2:tau.t3:c!false.disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "t3:c!true.disc()")]), (ActionTau 2, QDist [SubState empty (parseInferP "t3:c!false.disc()")])]
        SubState empty (parseInferP "(t1:tau.t3:c!true.disc()) + (disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "t3:c!true.disc()")])]
        SubState empty (parseInferP "(disc()) + (t2:tau.t3:c!false.disc())") `shouldSubStepTo` [(ActionTau 2, QDist [SubState empty (parseInferP "t3:c!false.disc()")])]
        SubState empty (parseInferP "((t1:tau.t3:c!true.disc()) + (t2:tau.t3:c!false.disc())) + (t4:tau.disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "t3:c!true.disc()")]), (ActionTau 2, QDist [SubState empty (parseInferP "t3:c!false.disc()")]), (ActionTau 4, QDist [SubState empty (parseInferP "disc()")])]
        SubState empty (parseInferP "(t1:c!true.disc()) + (disc())") `shouldSubStepTo` [(ActionSend 1 (Channel "c" TBool) (wd $ BoolLit True), QDist [SubState empty (parseInferP "disc()")])]
        SubState zero1 (parseInferP "(t1:MH(qb0>x).disc(qb0)) + (t2:tau.t3:c!false.disc(qb0))") `shouldSubStepTo` [(ActionTau 1, QDist [SubState i1 (parseInferP "disc(qb0)")]), (ActionTau 2, QDist [SubState zero1 (parseInferP "t3:c!false.disc(qb0)")])]
        SubState zero1 (parseInferP "(t1:MH(qb0>x).t3:d!x.disc(qb0)) + (t2:tau.t3:c!false.disc(qb0))") `shouldSubStepTo` [(ActionTau 1, QDist [SubState (scaleQS 0.5 plus1) (parseInferP "t3:d:bit!0.disc(qb0)"), SubState (scaleQS 0.5 minus1) (parseInferP "t3:d:bit!1.disc(qb0)")]), (ActionTau 2, QDist [SubState zero1 (parseInferP "t3:c!false.disc(qb0)")])]
    it "can step in parallel" $ do
        SubState empty (parseInferP "(t1:tau.disc()) || (t2:tau.disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "(disc()) || t2:tau.disc()")]), (ActionTau 2, QDist [SubState empty (parseInferP "(t1:tau.disc()) || disc()")])]
        SubState empty (parseInferP "(disc()) || (t2:tau.disc())") `shouldSubStepTo` [(ActionTau 2, QDist [SubState empty (parseInferP "(disc()) || disc()")])]
        SubState empty (parseInferP "(t1:tau.disc()) || (disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "(disc()) || disc()")])]
        SubState empty (parseInferP "((t1:tau.disc()) || (t2:tau.disc())) || t3:tau.disc()") `shouldSubStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "((disc()) || t2:tau.disc()) || t3:tau.disc()")]), (ActionTau 2, QDist [SubState empty (parseInferP "((t1:tau.disc()) || disc()) || t3:tau.disc()")]), (ActionTau 3, QDist [SubState empty (parseInferP "((t1:tau.disc()) || t2:tau.disc()) || disc()")])]
        SubState zero1 (parseInferP "(t1:MH(qb0>x).disc(qb0)) || (t2:tau.disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState i1 (parseInferP "(disc(qb0)) || t2:tau.disc()")]), (ActionTau 2, QDist [SubState zero1 (parseInferP "(t1:MH(qb0>x).disc(qb0)) || disc()")])]
        SubState zero1 (parseInferP "(t1:MH(qb0>x).t3:d!x.disc(qb0)) || (t2:tau.disc())") `shouldSubStepTo` [(ActionTau 1, QDist [SubState (scaleQS 0.5 plus1) (parseInferP "(t3:d:bit!0.disc(qb0)) || t2:tau.disc()"), SubState (scaleQS 0.5 minus1) (parseInferP "(t3:d:bit!1.disc(qb0)) || t2:tau.disc()")]), (ActionTau 2, QDist [SubState zero1 (parseInferP "(t1:MH(qb0>x).t3:d!x.disc(qb0)) || disc()")])]
    it "can synchronize" $ do
        SubState empty (parseInferP "(t1:c:bits 2!0.disc()) || (t2:c?x.t3:d:bits 2!x.disc())")
            `shouldSubStepTo` [ (ActionSend 1 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "(disc()) || t2:c:bits 2?x.t3:d:bits 2!x.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "(t1:c:bits 2!0.disc()) || t3:d:bits 2!0.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "(t1:c:bits 2!0.disc()) || t3:d:bits 2!1.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 2), QDist [SubState empty (parseInferP "(t1:c:bits 2!0.disc()) || t3:d:bits 2!2.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 3), QDist [SubState empty (parseInferP "(t1:c:bits 2!0.disc()) || t3:d:bits 2!3.disc()")])
                              , (ActionTau2 1 2, QDist [SubState empty (parseInferP "(disc()) || t3:d:bits 2!0.disc()")])
                              ]
        SubState empty (parseInferP "(t1:c:bits 2?x.t3:d:bits 2!x.disc()) || (t2:c:bits 2!0.disc())")
            `shouldSubStepTo` [ (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "(t3:d:bits 2!0.disc()) || t2:c:bits 2!0.disc()")])
                              , (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "(t3:d:bits 2!1.disc()) || t2:c:bits 2!0.disc()")])
                              , (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 2), QDist [SubState empty (parseInferP "(t3:d:bits 2!2.disc()) || t2:c:bits 2!0.disc()")])
                              , (ActionRecvE 1 (Channel "c" (TBits 2)) (wd $ IntLit 3), QDist [SubState empty (parseInferP "(t3:d:bits 2!3.disc()) || t2:c:bits 2!0.disc()")])
                              , (ActionSend 2 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "(t1:c:bits 2?x.t3:d:bits 2!x.disc()) || disc()")])
                              , (ActionTau2 1 2, QDist [SubState empty (parseInferP "(t3:d:bits 2!0.disc()) || disc()")])
                              ]
        SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || (t2:c?x.t3:d:bits 2!x.disc())) || (t3:e:bits 2!0.disc())")
            `shouldSubStepTo` [ (ActionSend 1 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "((disc()) || t2:c:bits 2?x.t3:d:bits 2!x.disc()) || t3:e:bits 2!0.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || t3:d:bits 2!0.disc()) || t3:e:bits 2!0.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || t3:d:bits 2!1.disc()) || t3:e:bits 2!0.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 2), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || t3:d:bits 2!2.disc()) || t3:e:bits 2!0.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 3), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || t3:d:bits 2!3.disc()) || t3:e:bits 2!0.disc()")])
                              , (ActionTau2 1 2, QDist [SubState empty (parseInferP "((disc()) || t3:d:bits 2!0.disc()) || t3:e:bits 2!0.disc()")])
                              , (ActionSend 3 (Channel "e" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || t2:c:bits 2?x.t3:d:bits 2!x.disc()) || disc()")])
                              ]
        SubState empty (parseInferP "((t1:c:bits 2!0.disc()) + t1:c:bits 2!1.disc()) || (t2:c?x.t3:d:bits 2!x.disc())")
            `shouldSubStepTo` [ (ActionSend 1 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "(disc()) || t2:c:bits 2?x.t3:d:bits 2!x.disc()")])
                              , (ActionSend 1 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "(disc()) || t2:c:bits 2?x.t3:d:bits 2!x.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) + t1:c:bits 2!1.disc()) || t3:d:bits 2!0.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) + t1:c:bits 2!1.disc()) || t3:d:bits 2!1.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 2), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) + t1:c:bits 2!1.disc()) || t3:d:bits 2!2.disc()")])
                              , (ActionRecvE 2 (Channel "c" (TBits 2)) (wd $ IntLit 3), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) + t1:c:bits 2!1.disc()) || t3:d:bits 2!3.disc()")])
                              , (ActionTau2 1 2, QDist [SubState empty (parseInferP "(disc()) || t3:d:bits 2!0.disc()")])
                              , (ActionTau2 1 2, QDist [SubState empty (parseInferP "(disc()) || t3:d:bits 2!1.disc()")])
                              ]
    it "can restrict channels" $ do
        SubState empty (parseInferP "(t1:c:bits 2!0.disc()) \\ c") `shouldSubStepTo` []
        SubState empty (parseInferP "((t1:c:bits 2!0.disc()) + t1:d:bits 2!0.disc()) \\ c") `shouldSubStepTo` [(ActionSend 1 (Channel "d" (TBits 2)) (wd $ IntLit 0), QDist [SubState empty (wd $ Restr (wd $ Discard []) (wd $ Channel "c" (TBits 2)))])]
        SubState empty (parseInferP "((t1:c:bits 2!0.disc()) || (t2:c?x.t3:d:bits 2!x.disc())) \\ c") `shouldSubStepTo` [(ActionTau2 1 2, QDist [SubState empty (wd $ Restr (parseInferP "((disc()) || t3:d:bits 2!0.disc())") (wd $ Channel "c" (TBits 2)))])]
        SubState empty (parseInferP "((t1:c:bits 2!0.disc()) \\ c) || t2:c:bits 2!1.disc()") `shouldSubStepTo` [(ActionSend 2 (Channel "c" (TBits 2)) (wd $ IntLit 1), QDist [SubState empty (parseInferP "((t1:c:bits 2!0.disc()) \\ c) || disc()")])]
    describe "distribution stepping" $ do
        it "can handle simple distributions" $ do
            QDist [SubState empty (parseInferP "t1:tau.disc()"), SubState empty (parseInferP "t1:tau.t2:tau.disc()")] `shouldStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "t2:tau.disc()"), SubState empty (parseInferP "disc()")])]
            QDist [SubState (scaleQS 0.5 zero1) (parseInferP "t1:tau.disc(qb0)"), SubState (scaleQS 0.5 one1) (parseInferP "t1:H(qb0).t2:tau.disc(qb0)")] `shouldStepTo` [(ActionTau 1, QDist [SubState (scaleQS 0.5 zero1) (parseInferP "disc(qb0)"), SubState (scaleQS 0.5 minus1) (parseInferP "t2:tau.disc(qb0)")])]
            QDist [SubState (scaleQS 0.5 plus1) (parseInferP "t1:tau.disc(qb0)"), SubState (scaleQS 0.5 one1) (parseInferP "t1:H(qb0).disc(qb0)")] `shouldStepTo` [(ActionTau 1, QDist [SubState i1 (parseInferP "disc(qb0)")])]
            QDist [SubState empty (parseInferP "t1:tau.disc()"), SubState empty (parseInferP "t2:tau.t3:tau.disc()")] `shouldStepTo` [(ActionTau 1, QDist [SubState empty (parseInferP "disc()")]), (ActionTau 2, QDist [SubState empty (parseInferP "t3:tau.disc()")])]
