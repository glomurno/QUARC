{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.TaggingSpec (spec) where

import Data.List (nub, sort)
import Data.Text (Text)
import Data.WithData (WithData (..), val)

import Common
import Hedgehog (Gen, annotateShow, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec

import LqCCS.Parser.File
import LqCCS.Tagging
import LqCCS.Types

tagsOf :: RawProcess meta -> [TagExpr]
tagsOf (WD _ p) = case p of
    RTau t k -> val t : tagsOf k
    RTau2 t1 t2 k -> val t1 : val t2 : tagsOf k
    RSend t _ _ _ k -> val t : tagsOf k
    RRecv t _ _ _ k -> val t : tagsOf k
    RApply t _ _ k -> val t : tagsOf k
    RMeas t _ _ _ k -> val t : tagsOf k
    RITE _ l r -> tagsOf l ++ tagsOf r
    RSum l r -> tagsOf l ++ tagsOf r
    RPar l r -> tagsOf l ++ tagsOf r
    RRestr k _ -> tagsOf k
    RDiscard _ -> []
    RRef{} -> []

branchTags :: Text -> [Integer]
branchTags src = [n | TConc n <- tagsOf (fst (retagProcess [] (parseP src)))]

parseWith :: TagPolicy -> String -> InputFileResult
parseWith policy = either error (applyTagPolicy policy) . parseFile

startTags :: InputFileResult -> [[TagExpr]]
startTags inp = [tagsOf e.deProc | b <- inp.inpStartBlocks, e <- b.sbEntries]

calledTags :: InputFileResult -> [[TagExpr]]
calledTags inp = [tagsOf body | WD _ (RawDecl (WD _ name) _ _ _ _ body) <- inp.inpDecls, name /= declDisplayName name]

spec :: Spec
spec = do
    propertySpec
    describe "branch tagging" $ do
        it "uses one tag for a linear sequence" $
            branchTags "tau.tau.disc()" `shouldBe` [-1, -1]
        it "keeps the tag through both branches of an if-then-else" $
            branchTags "tau.(if true then I(q0) else X(q0)).tau.disc(q0)" `shouldBe` [-1, -1, -1, -1, -1]
        it "uses a fresh tag in each branch of a parallel composition" $
            branchTags "tau.((tau.disc()) || (tau.disc()))" `shouldBe` [-1, -2, -3]
        it "uses a fresh tag in each branch of a sum" $
            branchTags "tau.((tau.disc()) + (tau.disc()))" `shouldBe` [-1, -2, -3]
        it "gives every operand of a nested sum its own tag, without gaps" $
            branchTags "tau.disc() + tau.disc() + tau.disc()" `shouldBe` [-1, -2, -3]
        it "gives each copy of a sugared parallel continuation its branch's tag" $
            branchTags "(tau || tau).tau.disc()" `shouldBe` [-1, -1, -2, -2]
        it "keeps manually placed tags" $
            branchTags "t5:tau.tau.(t7:tau.disc() || tau.t8:tau.tau.disc())" `shouldBe` [5, -1, 7, -2, 8, -2]
        it "restarts numbering in each start block" $ do
            let inp =
                    parseWith BranchTagging $
                        "start \"a\" { density 1 dm { 1,0; 0,0 } process tau.tau.disc() }\n"
                            ++ "start \"b\" { density 1 dm { 1,0; 0,0 } process tau.disc() }\n"
            startTags inp `shouldBe` [[TConc (-1), TConc (-1)], [TConc (-1)]]
        it "continues the caller's sequence into a called declaration" $ do
            let inp =
                    parseWith BranchTagging $
                        "proc A := tau.((tau.disc()) || (tau.disc()))\n"
                            ++ "proc B := tau.@A\n"
                            ++ "start { density 1 dm { 1,0; 0,0 } process tau.@B }\n"
            calledTags inp `shouldBe` [[TConc (-1), TConc (-2), TConc (-3)], [TConc (-1)]]
        it "tags each call site of a declaration apart" $ do
            let inp =
                    parseWith BranchTagging $
                        "proc A := tau.((tau.disc()) || (tau.disc()))\n"
                            ++ "start { density 1 dm { 1,0; 0,0 } process @A || @A || tau.@A }\n"
            calledTags inp
                `shouldBe` [ [TConc (-1), TConc (-2), TConc (-3)]
                           , [TConc (-4), TConc (-5), TConc (-6)]
                           , [TConc (-7), TConc (-8), TConc (-9)]
                           ]
            startTags inp `shouldBe` [[TConc (-7)]]
        it "reuses tag numbers across the branches of an if-then-else" $
            branchTags "if true then tau.((tau.disc()) || (tau.disc())) else ((tau.disc()) || (tau.disc()))"
                `shouldBe` [-1, -2, -3, -2, -3]
        it "never reuses the sequence tag for a fresh tag in the other branch" $ do
            branchTags "if true then tau.disc() else ((tau.disc()) || (tau.disc()))" `shouldBe` [-1, -2, -3]
            branchTags "if true then ((tau.disc()) || (tau.disc())) else tau.disc()" `shouldBe` [-2, -3, -1]
        it "tags calls in both branches of an if-then-else alike" $ do
            let inp =
                    parseWith BranchTagging $
                        "proc A := tau.((tau.disc()) || (tau.disc()))\n"
                            ++ "start { density 1 dm { 1,0; 0,0 } process if true then tau.@A else tau.@A }\n"
            calledTags inp `shouldBe` replicate 2 [TConc (-1), TConc (-2), TConc (-3)]
        it "names call-site copies after their declaration" $ do
            let inp = parseWith BranchTagging "proc A := tau.disc()\nstart { density 1 dm { 1,0; 0,0 } process @A || @A }\n"
                names = [declDisplayName name | WD _ (RawDecl (WD _ name) _ _ _ _ _) <- inp.inpDecls]
            names `shouldBe` ["A", "A", "A"]
        it "leaves recursive calls for the typechecker to reject" $ do
            let inp = parseWith BranchTagging "proc A := tau.@A\nstart { density 1 dm { 1,0; 0,0 } process @A }\n"
            length inp.inpDecls `shouldBe` 2
        it "keeps offset tags" $ do
            let inp = parseWith BranchTagging "proc A with tag_offset(b) := $b+1:tau.tau.disc()\nstart { density 1 dm { 1,0; 0,0 } process @A with b=10 }\n"
            calledTags inp `shouldBe` [[TOffset "b" 1, TConc (-1)]]

    describe "linear tagging" $
        it "gives every untagged action a fresh tag" $ do
            let inp = parseWith LinearTagging "start { density 1 dm { 1,0; 0,0 } process tau.tau.(tau.disc() || tau.disc()) }\n"
            let ts = concat (startTags inp)
            length ts `shouldBe` 4
            nub ts `shouldBe` ts

autos :: [TagExpr] -> [Integer]
autos ts = [n | TConc n <- ts, n < 0]

mapTags :: (TagExpr -> TagExpr) -> RawProcess meta -> RawProcess meta
mapTags f (WD m p) = WD m $ case p of
    RTau t k -> RTau (g t) (go k)
    RTau2 t1 t2 k -> RTau2 (g t1) (g t2) (go k)
    RSend t ch ty e k -> RSend (g t) ch ty e (go k)
    RRecv t ch ty v k -> RRecv (g t) ch ty v (go k)
    RApply t op es k -> RApply (g t) op es (go k)
    RMeas t ms es v k -> RMeas (g t) ms es v (go k)
    RITE e l r -> RITE e (go l) (go r)
    RSum l r -> RSum (go l) (go r)
    RPar l r -> RPar (go l) (go r)
    RRestr k ch -> RRestr (go k) ch
    other -> other
  where
    g = fmap f
    go = mapTags f

stretch :: RawProcess meta -> ([TagExpr], [(RawProcess meta, RawProcess meta)])
stretch (WD _ p) = case p of
    RTau t k -> val t `with` stretch k
    RTau2 t1 t2 k -> val t1 `with` (val t2 `with` stretch k)
    RSend t _ _ _ k -> val t `with` stretch k
    RRecv t _ _ _ k -> val t `with` stretch k
    RApply t _ _ k -> val t `with` stretch k
    RMeas t _ _ _ k -> val t `with` stretch k
    RITE _ l r -> stretch l <> stretch r
    RRestr k _ -> stretch k
    RSum l r -> ([], [(l, r)])
    RPar l r -> ([], [(l, r)])
    _ -> ([], [])
  where
    with t (ts, ops) = (t : ts, ops)

stretchesShareOneTag :: RawProcess meta -> Bool
stretchesShareOneTag p =
    length (nub here) <= 1
        && all (\(l, r) -> null ([t | t <- here, t `elem` autos (tagsOf l ++ tagsOf r)]) && stretchesShareOneTag l && stretchesShareOneTag r) ops
  where
    (ts, ops) = stretch p
    here = autos ts

operandsDisjoint :: RawProcess meta -> Bool
operandsDisjoint p = all (\(l, r) -> disjoint l r && operandsDisjoint l && operandsDisjoint r) (snd (stretch p))
  where
    disjoint l r = null [t | t <- autos (tagsOf l), t `elem` autos (tagsOf r)]

sprinkleTags :: RawProcess meta -> Gen (RawProcess meta)
sprinkleTags (WD m p) =
    WD m <$> case p of
        RTau t k -> RTau <$> g t <*> go k
        RTau2 t1 t2 k -> RTau2 <$> g t1 <*> g t2 <*> go k
        RSend t ch ty e k -> (\t' -> RSend t' ch ty e) <$> g t <*> go k
        RRecv t ch ty v k -> (\t' -> RRecv t' ch ty v) <$> g t <*> go k
        RApply t op es k -> (\t' -> RApply t' op es) <$> g t <*> go k
        RMeas t ms es v k -> (\t' -> RMeas t' ms es v) <$> g t <*> go k
        RITE e l r -> RITE e <$> go l <*> go r
        RSum l r -> RSum <$> go l <*> go r
        RPar l r -> RPar <$> go l <*> go r
        RRestr k ch -> (`RRestr` ch) <$> go k
        other -> pure other
  where
    g (WD l t) = WD l <$> Gen.frequency [(3, pure t), (1, TConc <$> Gen.integral (Range.constant 1 3))]
    go = sprinkleTags

genRawProcess :: Gen RawProcessFC
genRawProcess = do
    (_, src) <- genProcess
    sprinkleTags (parseP src)

branch :: RawProcessFC -> RawProcessFC
branch = fst . retagProcess []

propertySpec :: Spec
propertySpec = describe "properties" $ do
    describe "branch tagging" $ do
        it "only changes tags" $ checkProp $ property $ do
            p <- forAll genRawProcess
            let erase = mapTags (const (TConc 0))
            erase (branch p) === erase p
        it "is idempotent" $ checkProp $ property $ do
            p <- forAll genRawProcess
            branch (branch p) === branch p
        it "keeps explicit tags" $ checkProp $ property $ do
            p <- forAll genRawProcess
            [t | (t, TConc n) <- zip (tagsOf (branch p)) (tagsOf p), n >= 0] === [t | t@(TConc n) <- tagsOf p, n >= 0]
        it "numbers the automatic tags without gaps" $ checkProp $ property $ do
            p <- forAll genRawProcess
            let used = sort (nub (autos (tagsOf (branch p))))
            used === [negate (fromIntegral (length used)) .. -1]
        it "gives every linear stretch a single tag, fresh below it" $ checkProp $ property $ do
            p <- forAll genRawProcess
            annotateShow (tagsOf (branch p))
            assert (stretchesShareOneTag (branch p))
        it "gives the operands of a sum or parallel composition distinct tags" $ checkProp $ property $ do
            p <- forAll genRawProcess
            assert (operandsDisjoint (branch p))

    describe "linear tagging" $
        it "gives every untagged prefix its own tag" $
            checkProp $
                property $ do
                    p <- forAll genRawProcess
                    let ts = autos (tagsOf p)
                    nub ts === ts
