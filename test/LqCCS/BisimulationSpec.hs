{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.BisimulationSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.LTS
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.WithData

import Common
import Hedgehog (Gen, Property, PropertyT, annotateShow, assert, classify, forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec

import LqCCS.Bisimulation qualified as Bisim
import LqCCS.EvalTagged qualified as EV
import LqCCS.QDist
import LqCCS.Types

buildLTS :: QState -> ProcessFC -> EV.DecSemanticsFC
buildLTS qs p = orFail "eval error" $ EV.eval mempty (single qs p)

buildCombinedLTS :: QState -> ProcessFC -> QState -> ProcessFC -> EV.DecSemanticsFC
buildCombinedLTS qs1 p1 qs2 p2 = orFail "eval error" $ EV.evalPar [(mempty, single qs1 p1), (mempty, single qs2 p2)]

bisimilarInitials :: EV.DecSemanticsFC -> Bool
bisimilarInitials lts = case lts.val.ltsInitial of
    [s1, s2] -> Bisim.strongBisimilar lts s1 s2
    _ -> error "expected 2 initial states"

branchingInitialsUnder :: [(Tag, Tag)] -> Silence -> EV.DecSemanticsFC -> Either Bisim.BisimError Bool
branchingInitialsUnder pairs silence lts = case lts.val.ltsInitial of
    [s1, s2] -> Bisim.branchingBisimilarWith (Bisim.tagView pairs silence) lts s1 s2
    _ -> error "expected 2 initial states"

branchingInitialsWith :: [(Tag, Tag)] -> EV.DecSemanticsFC -> Either Bisim.BisimError Bool
branchingInitialsWith pairs = branchingInitialsUnder pairs mempty

branchingInitials :: EV.DecSemanticsFC -> Either Bisim.BisimError Bool
branchingInitials = branchingInitialsWith []

silencePair :: [(Tag, Tag)] -> Silence -> Text -> Text -> Either Bisim.BisimError Bool
silencePair pairs silence p q =
    branchingInitialsUnder pairs silence (buildCombinedLTS empty (parseInferTaggedP p) empty (parseInferTaggedP q))

branchingPairWith :: [(Tag, Tag)] -> QState -> Text -> Text -> Either Bisim.BisimError Bool
branchingPairWith pairs qs p q =
    branchingInitialsWith pairs (buildCombinedLTS qs (parseInferTaggedP p) qs (parseInferTaggedP q))

branchingPair :: QState -> Text -> Text -> Either Bisim.BisimError Bool
branchingPair = branchingPairWith []

rootedPairWith :: [(Tag, Tag)] -> QState -> Text -> Text -> Either Bisim.BisimError Bool
rootedPairWith pairs qs p q =
    let lts = buildCombinedLTS qs (parseInferTaggedP p) qs (parseInferTaggedP q)
     in case lts.val.ltsInitial of
            [s1, s2] -> Bisim.rootedBranchingBisimilarWith (Bisim.normView (Bisim.buildTagNorm pairs)) lts s1 s2
            _ -> error "expected 2 initial states"

rootedPair :: QState -> Text -> Text -> Either Bisim.BisimError Bool
rootedPair = rootedPairWith []

auto :: Integer -> Tag
auto n = ActionTag (negate n)

afterBlocked :: Text -> Text
afterBlocked p = "((r:bit!0.disc()) \\ r) || (" <> p <> ")"

strongRefinesBranching :: EV.DecSemanticsFC -> Bool
strongRefinesBranching lts = case Bisim.branchingBisimilarityClassesWith (Bisim.normView id) lts of
    Left _ -> False
    Right bclasses -> all (\c -> any (c `IS.isSubsetOf`) bclasses) (Bisim.strongBisimilarityClasses lts)

spec :: Spec
spec = do
    strongSpec
    branchingSpec
    propertySpec

strongSpec :: Spec
strongSpec = describe "strong bisimulation" $ do
    describe "reflexivity" $ do
        it "trivial process is bisimilar to itself" $ do
            let lts = buildLTS empty (parseInferTaggedP "disc()")
            case lts.val.ltsInitial of
                [s0] -> Bisim.strongBisimilar lts s0 s0 `shouldBe` True
                _ -> expectationFailure "expected exactly one initial state"
        it "all states in a chain are bisimilar to themselves" $ do
            let lts = buildLTS empty (parseInferTaggedP "tau.tau.disc()")
            mapM_ (\s -> Bisim.strongBisimilar lts s s `shouldBe` True) (Bisim.stateIndices lts)

    describe "bisimilar processes" $ do
        it "identical empty processes are bisimilar" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "disc()") empty (parseInferTaggedP "disc()"))
                `shouldBe` True
        it "identical silent processes are bisimilar" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "tau.disc()") empty (parseInferTaggedP "tau.disc()"))
                `shouldBe` True
        it "identical send processes are bisimilar" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "c!0.disc()") empty (parseInferTaggedP "c!0.disc()"))
                `shouldBe` True
        it "a send of open width matches the same send on a fixed-width channel" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "c!0.disc()") empty (parseInferTaggedP "c:bit!0.disc()"))
                `shouldBe` True
        it "sends on channels of different fixed widths stay distinct" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "c:bits 2!0.disc()") empty (parseInferTaggedP "c:bit!0.disc()"))
                `shouldBe` False
        it "identical quantum gate processes are bisimilar" $ do
            bisimilarInitials (buildCombinedLTS zero1 (parseInferTaggedP "X(qb0).disc(qb0)") zero1 (parseInferTaggedP "X(qb0).disc(qb0)"))
                `shouldBe` True
        it "invisible but different quantum gate processes are bisimilar" $ do
            bisimilarInitials (buildCombinedLTS zero1 (parseInferTaggedP "X(qb0).disc(qb0)") zero1 (parseInferTaggedP "H(qb0).disc(qb0)"))
                `shouldBe` True

    describe "non-bisimilar processes" $ do
        it "processes sending different values are not bisimilar" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "c!0.disc()") empty (parseInferTaggedP "c!1.disc()"))
                `shouldBe` False
        it "process with tau and without are not bisimilar" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "tau.disc()") empty (parseInferTaggedP "disc()"))
                `shouldBe` False
        it "processes sending on different channels are not bisimilar" $ do
            bisimilarInitials (buildCombinedLTS empty (parseInferTaggedP "c!0.disc()") empty (parseInferTaggedP "d!0.disc()"))
                `shouldBe` False
        it "processes with different but visible quantum gates are not bisimilar" $ do
            bisimilarInitials (buildCombinedLTS zero1 (parseInferTaggedP "X(qb0).c!qb0.disc()") zero1 (parseInferTaggedP "H(qb0).c!qb0.disc()"))
                `shouldBe` False

    describe "one-pass algorithm" $ do
        let refine lts = Bisim.classesOf (Bisim.strongBisimilarityRefine id lts (Bisim.groupByEnv lts))
            agrees lts = canon (Bisim.strongBisimilarityClasses lts) `shouldBe` canon (refine lts)
        it "agrees with partition refinement on single processes" $
            mapM_
                (agrees . buildLTS zero1 . parseInferTaggedP)
                [ "tau.tau.disc(qb0)"
                , "(tau.disc(qb0)) + (tau.if 0=0 then disc(qb0) else disc(qb0))"
                , "(c!0.disc(qb0)) + (tau.((c!0.disc(qb0)) + (d!0.disc(qb0))))"
                , "MH(qb0>x).c!x.disc(qb0)"
                , "X(qb0).c!qb0.disc()"
                ]
        it "agrees with partition refinement on composed processes" $ do
            agrees (buildCombinedLTS zero1 (parseInferTaggedP "X(qb0).disc(qb0)") zero1 (parseInferTaggedP "H(qb0).disc(qb0)"))
            agrees (buildCombinedLTS zero1 (parseInferTaggedP "X(qb0).c!qb0.disc()") zero1 (parseInferTaggedP "H(qb0).c!qb0.disc()"))
            agrees (buildCombinedLTS empty (parseInferTaggedP "tau.c!0.disc()") empty (parseInferTaggedP "c!0.disc()"))
        it "agrees with partition refinement when a split block must split again" $ do
            let lts = buildLTS empty (parseInferTaggedP "(tau.disc()) + (tau.tau.disc())")
                norm = Bisim.buildTagNorm [(auto 1, auto 2)]
            canon (Bisim.classesOf (Bisim.strongBisimilarityNorm norm lts))
                `shouldBe` canon (Bisim.classesOf (Bisim.strongBisimilarityRefine norm lts (Bisim.groupByEnv lts)))

    describe "minimalLTS" $ do
        it "does not drop transitions when two edges collapse onto the same class pair" $ do
            let lts = buildLTS zero1 (parseInferTaggedP "(tau.disc(qb0)) + (tau.if 0=0 then disc(qb0) else disc(qb0))")
                minLts = Bisim.minimalLTS lts
            s0 <- onlyOf "initial state" lts.val.ltsInitial
            c0 <- firstOf "class containing the initial state" [idx | (cls, idx) <- zip (Bisim.strongBisimilarityClasses lts) [0 :: Int ..], s0 `IS.member` cls]
            let outActions = sum [HS.size acts | ((s, _), acts) <- HM.toList minLts.ltsTransitions, s == c0]
            outActions `shouldBe` 2

branchingSpec :: Spec
branchingSpec = describe "branching bisimulation" $ do
    describe "bisimilar processes" $ do
        it "a leading tau is silent" $ do
            branchingPair empty "tau.c!0.disc()" "c!0.disc()" `shouldBe` Right True
        it "a chain of taus is silent" $ do
            branchingPair empty "tau.tau.c!0.disc()" "c!0.disc()" `shouldBe` Right True
        it "tau tags are erased" $ do
            branchingPair empty "tau.disc()" (afterBlocked "tau.disc()") `shouldBe` Right True
        it "a silent step inside a summand is silent when it preserves the other options" $ do
            branchingPairWith
                [(auto 3, auto 1), (auto 4, auto 2)]
                empty
                "(c!0.disc()) + (tau.((c!0.disc()) + (d!0.disc())))"
                "(c!0.disc()) + (d!0.disc())"
                `shouldBe` Right True
        it "invisible quantum gates are silent" $ do
            branchingPair zero1 "X(qb0).c!0.disc(qb0)" "c!0.disc(qb0)" `shouldBe` Right True
        it "is coarser than strong bisimilarity" $ do
            let lts = buildCombinedLTS empty (parseInferTaggedP "tau.c!0.disc()") empty (parseInferTaggedP "c!0.disc()")
            bisimilarInitials lts `shouldBe` False
            branchingInitials lts `shouldBe` Right True
            strongRefinesBranching lts `shouldBe` True

    describe "non-bisimilar processes" $ do
        it "processes sending different values are not bisimilar" $ do
            branchingPair empty "tau.c!0.disc()" "c!1.disc()" `shouldBe` Right False
        it "visible actions keep their tags" $ do
            branchingPair empty "(c!0.disc()) + (d!0.disc())" "(d!0.disc()) + (c!0.disc())" `shouldBe` Right False
        it "a tau action that discards an option is not inert" $ do
            branchingPair
                empty
                "(c!0.disc()) + (tau.d!0.disc())"
                "(c!0.disc()) + (d!0.disc())"
                `shouldBe` Right False
        it "separates branching from weak bisimilarity" $ do
            let lts =
                    buildCombinedLTS
                        empty
                        (parseInferTaggedP "(c!0.disc()) + (tau.d!0.disc())")
                        empty
                        (parseInferTaggedP "(c!0.disc()) + (tau.d!0.disc()) + (d!0.disc())")
            branchingInitialsWith [(auto 2, auto 3)] lts `shouldBe` Right False
        it "a silent gate is observable through the environment once its qubit is released" $ do
            branchingPair zero1 "X(qb0).c!qb0.disc()" "c!qb0.disc()" `shouldBe` Right False

    describe "tag equivalence" $ do
        it "identifies visible actions with equivalent tags" $ do
            let lts = buildCombinedLTS empty (parseInferTaggedP "tau.((c!0.disc()) + (d!0.disc()))") empty (parseInferTaggedP "(d!0.disc()) + (c!0.disc())")
            branchingInitials lts `shouldBe` Right False
            branchingInitialsWith [(auto 3, auto 1)] lts `shouldBe` Right True

    describe "minimization" $ do
        it "collapses a tau chain into one state without self-loops" $ do
            case Bisim.branchingMinimalLTSWith (Bisim.normView id) (buildLTS empty (parseInferTaggedP "tau.tau.disc()")) of
                Left e -> expectationFailure (show e)
                Right m -> do
                    HM.size m.ltsStates `shouldBe` 1
                    HM.size m.ltsTransitions `shouldBe` 0
        it "keeps the visible step after an silent tau" $ do
            case Bisim.branchingMinimalLTSWith (Bisim.normView id) (buildLTS empty (parseInferTaggedP "tau.c!0.disc()")) of
                Left e -> expectationFailure (show e)
                Right m -> do
                    HM.size m.ltsStates `shouldBe` 2
                    sum (HS.size <$> HM.elems m.ltsTransitions) `shouldBe` 1

    describe "rooted branching bisimulation" $ do
        it "does not skip an initial tau" $ do
            branchingPair empty "tau.c!0.disc()" "c!0.disc()" `shouldBe` Right True
            rootedPair empty "tau.c!0.disc()" "c!0.disc()" `shouldBe` Right False
        it "matches an initial tau by a tau with a different tag" $ do
            rootedPair empty "tau.disc()" (afterBlocked "tau.disc()") `shouldBe` Right True
        it "skips taus after the first step" $ do
            rootedPair empty "tau.tau.c!0.disc()" "tau.c!0.disc()" `shouldBe` Right True
        it "distinguishes a + tau.(a + b) from a + b" $ do
            let p = "(c!0.disc()) + (tau.((c!0.disc()) + (d!0.disc())))"
                q = "(c!0.disc()) + (d!0.disc())"
            branchingPairWith [(auto 3, auto 1), (auto 4, auto 2)] empty p q `shouldBe` Right True
            rootedPairWith [(auto 3, auto 1), (auto 4, auto 2)] empty p q `shouldBe` Right False
        it "identifies invisible initial gates" $ do
            rootedPair zero1 "X(qb0).c!0.disc(qb0)" "H(qb0).c!0.disc(qb0)" `shouldBe` Right True
        it "agrees with strong bisimilarity when there are no silent steps" $ do
            rootedPair empty "c!0.disc()" "c!0.disc()" `shouldBe` Right True
            rootedPair empty "c!0.disc()" "c!1.disc()" `shouldBe` Right False
        it "is implied by strong bisimilarity" $ do
            let lts = buildCombinedLTS zero1 (parseInferTaggedP "X(qb0).c!qb0.disc()") zero1 (parseInferTaggedP "X(qb0).c!qb0.disc()")
            bisimilarInitials lts `shouldBe` True
            case lts.val.ltsInitial of
                [s1, s2] -> Bisim.rootedBranchingBisimilarWith (Bisim.normView id) lts s1 s2 `shouldBe` Right True
                _ -> expectationFailure "expected 2 initial states"
        it "leaves strong checks unchanged under checkBisimilar" $ do
            let lts = buildCombinedLTS empty (parseInferTaggedP "tau.c!0.disc()") empty (parseInferTaggedP "c!0.disc()")
            case lts.val.ltsInitial of
                [s1, s2] -> Bisim.checkBisimilar Bisim.Strong Bisim.Rooted (Bisim.normView id) lts s1 s2 `shouldBe` Right False
                _ -> expectationFailure "expected 2 initial states"

    describe "silent and visible tags" $ do
        let leading = "tau.c!0.disc()"
            plain = "c!0.disc()"
        it "makes every tau tag silent by default" $
            silencePair [] mempty leading plain `shouldBe` Right True
        it "keeps a visible tag observable" $
            silencePair [] (Silence [] [auto 1]) leading plain `shouldBe` Right False
        it "keeps tau tags outside a silent list observable" $ do
            silencePair [] (Silence [auto 3] []) leading plain `shouldBe` Right False
            silencePair [] (Silence [auto 1] []) leading plain `shouldBe` Right True
        it "removes visible tags from the silent list" $
            silencePair [] (Silence [auto 1] [auto 1]) leading plain `shouldBe` Right False
        it "matches an observable tau against the same tag" $ do
            let visible = Silence [] [auto 1, auto 2]
            silencePair [] visible "tau.disc()" "tau.disc()" `shouldBe` Right True
            silencePair [] visible "tau.disc()" (afterBlocked "tau.disc()") `shouldBe` Right False
        it "applies to synchronisation tags" $ do
            let synch = "((r:bit!0.c!0.disc()) || (r:bit?x.disc())) \\ r"
            silencePair [] mempty synch plain `shouldBe` Right True
            silencePair [] (Silence [] [SynchTag (-2) (-1)]) synch plain `shouldBe` Right False
        it "decides silence on tag-equivalence classes" $ do
            silencePair [(auto 1, auto 4)] (Silence [] [auto 4]) leading plain `shouldBe` Right False
            silencePair [(auto 1, auto 4)] (Silence [auto 4] []) leading plain `shouldBe` Right True
        it "keeps observable taus in the quotient" $ do
            let lts = buildLTS empty (parseInferTaggedP "tau.((tau.disc()) || (disc()))")
            case Bisim.branchingMinimalLTSWith (Bisim.tagView [] (Silence [] [auto 2])) lts of
                Left e -> expectationFailure (show e)
                Right m -> do
                    HM.size m.ltsStates `shouldBe` 2
                    sum (HS.size <$> HM.elems m.ltsTransitions) `shouldBe` 1

    describe "reverseTopoOrder" $ do
        let tau n = Bisim.NormTau (ActionTag n) :: Bisim.NormAction '[LocKV]
        it "places every state after its successors" $ do
            let fwd = IM.fromList [(0, [(tau 1, 1), (tau 2, 2)]), (1, [(tau 3, 2)])]
            Bisim.reverseTopoOrder fwd [0, 1, 2] `shouldBe` Right [2, 1, 0]
        it "reports a cycle" $ do
            let fwd = IM.fromList [(0, [(tau 1, 1)]), (1, [(tau 2, 0)])]
            Bisim.reverseTopoOrder fwd [0, 1] `shouldSatisfy` either (const True) (const False)

genPairLTS :: Gen EV.DecSemanticsFC
genPairLTS = do
    (qs1, p1) <- genProcess
    let tau t p = "t" <> T.pack (show t) <> ":tau.(" <> p <> ")"
    tag <- Gen.int (Range.constant 1 4)
    (q, (qs2, p2)) <-
        Gen.choice
            [ pure (p1, (qs1, p1))
            , pure (p1, (qs1, tau tag p1))
            , pure (tau tag p1, (qs1, tau (tag + 4) p1))
            , (p1,) <$> genProcess
            ]
    pure (buildCombinedLTS qs1 (parseInferTaggedP q) qs2 (parseInferTaggedP p2))

ltsProperty :: PropertyT IO () -> Property
ltsProperty = H.withTests 300 . property

forAllLTS :: PropertyT IO EV.DecSemanticsFC
forAllLTS = do
    lts <- forAll genPairLTS
    let size = HM.size lts.val.ltsStates
    annotateShow (size, HM.size lts.val.ltsTransitions)
    classify "at least 10 states" (size >= 10)
    classify "at least 50 states" (size >= 50)
    pure lts

genTag :: Gen Tag
genTag = Gen.frequency [(3, ActionTag <$> Gen.integral (Range.constant 1 6)), (1, SynchTag <$> Gen.integral (Range.constant 1 4) <*> Gen.integral (Range.constant 1 4))]

genTagPairs :: Gen [(Tag, Tag)]
genTagPairs = Gen.list (Range.linear 0 5) ((,) <$> genTag <*> genTag)

canon :: [IS.IntSet] -> [[Int]]
canon = sort . map IS.toList

isPartitionOf :: [IS.IntSet] -> EV.DecSemanticsFC -> Bool
isPartitionOf classes lts =
    not (any IS.null classes)
        && sum (IS.size <$> classes) == length states
        && IS.unions classes == IS.fromList states
  where
    states = Bisim.stateIndices lts

sameEnv :: EV.DecSemanticsFC -> IS.IntSet -> Bool
sameEnv lts c = HS.size (HS.fromList [env IM.! s | s <- IS.toList c]) == 1
  where
    env = Bisim.envKeys lts

refines :: [IS.IntSet] -> [IS.IntSet] -> Bool
refines fine coarse = all (\c -> any (c `IS.isSubsetOf`) coarse) fine

quotientClass :: Bisim.MinimalSemantics meta -> IM.IntMap Int
quotientClass m = IM.fromList [(s, w.val) | (members, w) <- HM.toList m.ltsStates, s <- members]

projectTransitions :: (Int -> Int -> Bisim.NormAction '[LocKV] -> Bool) -> EV.DecSemanticsFC -> IM.IntMap Int -> HM.HashMap (Int, Int) (HS.HashSet (Bisim.NormAction '[LocKV]))
projectTransitions dropped lts cls =
    HM.filter (not . HS.null) $
        HM.fromListWith
            HS.union
            [ ((cs, ct), HS.filter (not . dropped cs ct) (HS.map (Bisim.toNormAction id) acts))
            | ((s, t), acts) <- HM.toList lts.val.ltsTransitions
            , let cs = cls IM.! s
                  ct = cls IM.! t
            ]

closure :: [(Tag, Tag)] -> Tag -> [Tag]
closure pairs t0 = go [t0]
  where
    go seen =
        let next = [b | (a, b) <- sym, a `elem` seen, b `notElem` seen]
         in if null next then seen else go (seen ++ next)
    sym = pairs ++ [(b, a) | (a, b) <- pairs]

genDag :: Gen (Int, Bisim.FwdMap '[LocKV])
genDag = do
    n <- Gen.int (Range.linear 1 10)
    edges <- Gen.subsequence [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]
    pure (n, IM.fromListWith (++) [(i, [(Bisim.NormTau (ActionTag 1), j)]) | (i, j) <- edges])

propertySpec :: Spec
propertySpec = describe "properties" $ do
    describe "strong bisimilarity" $ do
        it "partitions the states" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            assert (Bisim.strongBisimilarityClasses lts `isPartitionOf` lts)
        it "never relates states with different environments" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            assert $ all (sameEnv lts) (Bisim.strongBisimilarityClasses lts)
        it "agrees with partition refinement under any tag equivalence" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            pairs <- forAll genTagPairs
            let norm = Bisim.buildTagNorm pairs
            canon (Bisim.classesOf (Bisim.strongBisimilarityNorm norm lts))
                === canon (Bisim.classesOf (Bisim.strongBisimilarityRefine norm lts (Bisim.groupByEnv lts)))
        it "becomes coarser as more tags are identified" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            pairs <- forAll genTagPairs
            extra <- forAll genTagPairs
            assert $
                Bisim.strongBisimilarityClassesWith (Bisim.buildTagNorm pairs) lts
                    `refines` Bisim.strongBisimilarityClassesWith (Bisim.buildTagNorm (pairs ++ extra)) lts
        it "minimized LTS keeps exactly the projected transitions" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            let m = Bisim.minimalLTS lts
            HM.size m.ltsStates === length (Bisim.strongBisimilarityClasses lts)
            m.ltsTransitions === projectTransitions (\_ _ _ -> False) lts (quotientClass m)

    describe "branching bisimilarity" $ do
        it "partitions the states" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            fmap (`isPartitionOf` lts) (Bisim.branchingBisimilarityClassesWith (Bisim.normView id) lts) === Right True
        it "never relates states with different environments" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            fmap (all (sameEnv lts)) (Bisim.branchingBisimilarityClassesWith (Bisim.normView id) lts) === Right True
        it "is coarser than strong bisimilarity" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            assert (strongRefinesBranching lts)
        it "equals strong bisimilarity when no tag is silent" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            pairs <- forAll genTagPairs
            let norm = Bisim.buildTagNorm pairs
            fmap canon (Bisim.branchingBisimilarityClassesWith (Bisim.TagView norm (const False)) lts)
                === Right (canon (Bisim.classesOf (Bisim.strongBisimilarityRefine norm lts (Bisim.groupByEnv lts))))
        it "becomes coarser as more tags are identified" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            pairs <- forAll genTagPairs
            extra <- forAll genTagPairs
            let classes ps = Bisim.branchingBisimilarityClassesWith (Bisim.tagView ps mempty) lts
            (refines <$> classes pairs <*> classes (pairs ++ extra)) === Right True
        it "minimized LTS keeps exactly the projected non-silent transitions" $ checkProp $ ltsProperty $ do
            lts <- forAllLTS
            case Bisim.branchingMinimalLTSWith (Bisim.normView id) lts of
                Left e -> fail (show e)
                Right m -> do
                    let inert cs ct a = cs == ct && Bisim.isSilent (Bisim.normView id) a
                    m.ltsTransitions === projectTransitions inert lts (quotientClass m)

    describe "rooted branching bisimilarity" $
        it "lies between strong and branching bisimilarity" $
            checkProp $
                ltsProperty $ do
                    lts <- forAllLTS
                    case lts.val.ltsInitial of
                        [s1, s2] -> do
                            let tv = Bisim.normView id
                                strong = Bisim.strongBisimilar lts s1 s2
                            rooted <- either (fail . show) pure (Bisim.rootedBranchingBisimilarWith tv lts s1 s2)
                            branching <- either (fail . show) pure (Bisim.branchingBisimilarWith tv lts s1 s2)
                            annotateShow (strong, rooted, branching)
                            classify "strongly bisimilar" strong
                            classify "rooted but not strongly bisimilar" (rooted && not strong)
                            classify "branching but not rooted bisimilar" (branching && not rooted)
                            classify "not branching bisimilar" (not branching)
                            assert ((not strong || rooted) && (not rooted || branching))
                        _ -> fail "expected 2 initial states"

    describe "buildTagNorm" $
        it "maps tags to the least tag of their equivalence class" $
            checkProp $
                property $ do
                    pairs <- forAll genTagPairs
                    t <- forAll genTag
                    u <- forAll genTag
                    let norm = Bisim.buildTagNorm pairs
                    norm (norm t) === norm t
                    norm t === minimum (closure pairs t)
                    (norm t == norm u) === (u `elem` closure pairs t)

    describe "reverseTopoOrder" $ do
        it "orders every state of a DAG after its successors" $ checkProp $ property $ do
            (n, fwd) <- forAll genDag
            case Bisim.reverseTopoOrder fwd [0 .. n - 1] of
                Left e -> fail (show e)
                Right order -> do
                    sort order === [0 .. n - 1]
                    let pos = IM.fromList (zip order [0 :: Int ..])
                    assert $ and [pos IM.! j < pos IM.! i | (i, outs) <- IM.toList fwd, (_, j) <- outs]
        it "reports a cycle through a back edge" $ checkProp $ property $ do
            (n, fwd) <- forAll genDag
            case [(i, j) | (i, outs) <- IM.toList fwd, (_, j) <- outs] of
                [] -> pure ()
                edges -> do
                    (i, j) <- forAll (Gen.element edges)
                    let cyclic = IM.insertWith (++) j [(Bisim.NormTau (ActionTag 1), i)] fwd
                    either (const True) (const False) (Bisim.reverseTopoOrder cyclic [0 .. n - 1]) === True
