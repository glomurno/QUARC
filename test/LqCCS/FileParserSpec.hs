{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.FileParserSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust, isNothing)

import Common
import Test.Hspec

import LqCCS.Parser.File
import LqCCS.QDist (QState (..))
import LqCCS.Types (Silence (..), Tag (..))

spec :: Spec
spec = do
    describe "start block parser" $ do
        it "parses a single-config start block with implicit state" $ do
            let src = "start {\n  density 1 H(0)\n  process disc()\n}\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- onlyOf "start block" inp.inpStartBlocks
                    isNothing block.sbName `shouldBe` True
                    length block.sbEntries `shouldBe` 1

        it "parses a single-config start block with explicit flat matrix" $ do
            let src = "start {\n  density 1 1,0,0,0\n  process disc()\n}\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- firstOf "start block" inp.inpStartBlocks
                    length block.sbEntries `shouldBe` 1

        it "parses a single-config start block with dm{} matrix" $ do
            let src = "start {\n  density 1 dm { 1,0; 0,0 }\n  process disc()\n}\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- firstOf "start block" inp.inpStartBlocks
                    entry <- firstOf "start entry" block.sbEntries
                    entry.deRho.dim `shouldBe` 1

        it "parses a distribution start block with =>" $ do
            let src =
                    unlines
                        [ "start {"
                        , "  density 1 dm { 0.5,0; 0,0 } => process disc()"
                        , "  density 1 dm { 0,0; 0,0.5 } => process disc()"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- firstOf "start block" inp.inpStartBlocks
                    length block.sbEntries `shouldBe` 2

        it "parses a named start block" $ do
            let src = "start \"alice\" {\n  density 1 H(0)\n  process disc()\n}\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- firstOf "start block" inp.inpStartBlocks
                    block.sbName `shouldBe` Just "alice"

        it "parses multiple named start blocks" $ do
            let src =
                    unlines
                        [ "start \"honest\" {"
                        , "  density 1 H(0)"
                        , "  process disc()"
                        , "}"
                        , "start \"adversary\" {"
                        , "  density 1 H(0)"
                        , "  process disc()"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    length inp.inpStartBlocks `shouldBe` 2
                    map (.sbName) inp.inpStartBlocks `shouldBe` [Just "honest", Just "adversary"]

        it "inpDefaultEntry returns the first unnamed block's entry" $ do
            let src = "start {\n  density 1 H(0)\n  process disc()\n}\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> isJust (inpDefaultEntry inp) `shouldBe` True

        it "inpDefaultEntry falls back to first block when all are named" $ do
            let src = "start \"x\" {\n  density 1 H(0)\n  process disc()\n}\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> isJust (inpDefaultEntry inp) `shouldBe` True

        it "inpDefaultEntry prefers unnamed block over named" $ do
            let src =
                    unlines
                        [ "start \"named\" {"
                        , "  density 1 H(0)"
                        , "  process disc()"
                        , "}"
                        , "start {"
                        , "  density 1 dm { 0,0; 0,1 }"
                        , "  process disc()"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    case inpDefaultEntry inp of
                        Nothing -> expectationFailure "expected default entry"
                        Just entry -> entry.deRho.dim `shouldBe` 1

    describe "dm{} density matrix" $ do
        it "accepts semicolons as row separators" $ do
            let src =
                    unlines
                        [ "start {"
                        , "  density 1 dm {"
                        , "    1,0;"
                        , "    0,0"
                        , "  }"
                        , "  process disc()"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp ->
                    isJust (inpDefaultEntry inp) `shouldBe` True

        it "accepts 2-qubit dm{} matrix" $ do
            let src =
                    unlines
                        [ "start {"
                        , "  density 2 dm {"
                        , "    0.25,0,0,0.25;"
                        , "    0,0,0,0;"
                        , "    0,0,0,0;"
                        , "    0.25,0,0,0.25"
                        , "  }"
                        , "  process disc()"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- firstOf "start block" inp.inpStartBlocks
                    entry <- firstOf "start entry" block.sbEntries
                    entry.deRho.dim `shouldBe` 2

    describe "silent and visible blocks" $ do
        let parsed extra = parseFile (unlines (["start \"a\" { density 1 H(0)  process disc() }", "start \"b\" { density 1 H(0)  process disc() }"] ++ extra))
        it "defaults to every tau tag being silent" $
            case parsed [] of
                Left err -> expectationFailure err
                Right inp -> inp.inpSilence `shouldBe` mempty
        it "parses a global silent block with plain and synch tags" $
            case parsed ["silent { 1 2 (10, 20) }"] of
                Left err -> expectationFailure err
                Right inp -> inp.inpSilence `shouldBe` Silence [ActionTag 1, ActionTag 2, SynchTag 10 20] []
        it "parses a global visible block" $
            case parsed ["visible {", "  7", "}"] of
                Left err -> expectationFailure err
                Right inp -> inp.inpSilence `shouldBe` Silence [] [ActionTag 7]
        it "combines several blocks" $
            case parsed ["silent { 1 }", "visible { 2 }", "silent { 3 }"] of
                Left err -> expectationFailure err
                Right inp -> inp.inpSilence `shouldBe` Silence [ActionTag 1, ActionTag 3] [ActionTag 2]
        it "parses scoped blocks" $
            case parsed ["visible \"a\" \"b\" { 5 }"] of
                Left err -> expectationFailure err
                Right inp -> do
                    inp.inpSilence `shouldBe` mempty
                    inp.inpScopedSilence `shouldBe` [("a", "b", Silence [] [ActionTag 5])]
        it "accepts blocks before the start blocks" $
            case parseFile (unlines ["silent { 4 }", "start { density 1 H(0)  process disc() }"]) of
                Left err -> expectationFailure err
                Right inp -> inp.inpSilence `shouldBe` Silence [ActionTag 4] []

    describe "tag-equiv block" $ do
        it "parses an empty tag-equiv block" $ do
            let src =
                    unlines
                        [ "start { density 1 H(0)  process disc() }"
                        , "tag-equiv {}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpTagEquiv `shouldBe` []

        it "parses a simple ActionTag equivalence" $ do
            let src =
                    unlines
                        [ "start { density 1 H(0)  process disc() }"
                        , "tag-equiv { 10 ~ 30 }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpTagEquiv `shouldBe` [(ActionTag 10, ActionTag 30)]

        it "parses a SynchTag equivalence" $ do
            let src =
                    unlines
                        [ "start { density 1 H(0)  process disc() }"
                        , "tag-equiv { (10, 20) ~ (30, 40) }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpTagEquiv `shouldBe` [(SynchTag 10 20, SynchTag 30 40)]

        it "parses multiple equivalence pairs" $ do
            let src =
                    unlines
                        [ "start { density 1 H(0)  process disc() }"
                        , "tag-equiv {"
                        , "  10 ~ 30"
                        , "  20 ~ 40"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpTagEquiv `shouldBe` [(ActionTag 10, ActionTag 30), (ActionTag 20, ActionTag 40)]

        it "parses mixed ActionTag and SynchTag equivalences" $ do
            let src =
                    unlines
                        [ "start { density 1 H(0)  process disc() }"
                        , "tag-equiv {"
                        , "  10 ~ 30"
                        , "  (10, 20) ~ (30, 40)"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp ->
                    inp.inpTagEquiv
                        `shouldBe` [ (ActionTag 10, ActionTag 30)
                                   , (SynchTag 10 20, SynchTag 30 40)
                                   ]

        it "parses tag-equiv with line comments" $ do
            let src =
                    unlines
                        [ "start { density 1 H(0)  process disc() }"
                        , "tag-equiv {"
                        , "  10 ~ 30  -- send action equivalence"
                        , "  (10, 20) ~ (30, 40)  -- synch pair equivalence"
                        , "}"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> length inp.inpTagEquiv `shouldBe` 2

        it "parses tag-equiv block before start block" $ do
            let src =
                    unlines
                        [ "tag-equiv { 10 ~ 30 }"
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpTagEquiv `shouldBe` [(ActionTag 10, ActionTag 30)]

        it "collects tag-equiv from before and after start block" $ do
            let src =
                    unlines
                        [ "tag-equiv { 10 ~ 30 }"
                        , "start { density 1 H(0)  process disc() }"
                        , "tag-equiv { 20 ~ 40 }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp ->
                    inp.inpTagEquiv
                        `shouldBe` [(ActionTag 10, ActionTag 30), (ActionTag 20, ActionTag 40)]

        it "inpTagEquiv is empty when no tag-equiv block is present" $ do
            let src = "start { density 1 H(0)  process disc() }\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpTagEquiv `shouldBe` []

    describe "import directive" $ do
        it "parses a single import and records the path in inpImports" $ do
            let src =
                    unlines
                        [ "import \"lib/bell.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpImports `shouldBe` ["lib/bell.quarc"]

        it "parses multiple imports in order" $ do
            let src =
                    unlines
                        [ "import \"a.quarc\""
                        , "import \"b.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpImports `shouldBe` ["a.quarc", "b.quarc"]

        it "imports may appear after other top-level items" $ do
            let src =
                    unlines
                        [ "proc Foo() := disc()"
                        , "import \"extra.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    inp.inpImports `shouldBe` ["extra.quarc"]
                    length inp.inpDecls `shouldBe` 1

        it "inpImports is empty when no import directive is present" $ do
            let src = "start { density 1 H(0)  process disc() }\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpImports `shouldBe` []

    describe "named state definitions" $ do
        it "parses a state definition with an implicit (operator sequence) state" $ do
            let src =
                    unlines
                        [ "state Bell := density 2 H(0).CNOT(0,1)"
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    HM.member "Bell" inp.inpNamedStates `shouldBe` True

        it "parses a state definition with a dm{} density matrix" $ do
            let src =
                    unlines
                        [ "state Mixed := density 1 dm { 0.5,0; 0,0.5 }"
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    HM.member "Mixed" inp.inpNamedStates `shouldBe` True
                    (HM.lookup "Mixed" inp.inpNamedStates >>= \s -> Just s.dim) `shouldBe` Just 1

        it "parses multiple state definitions" $ do
            let src =
                    unlines
                        [ "state A := density 1 H(0)"
                        , "state B := density 1 dm { 1,0; 0,0 }"
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp ->
                    length inp.inpNamedStates `shouldBe` 2

        it "rejects a state definition referencing an unknown operator" $ do
            let src =
                    unlines
                        [ "state Bad := density 1 Unknown(0)"
                        , "start { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected parse/resolution failure for unknown operator"

        it "inpNamedStates is empty when no state definitions are present" $ do
            let src = "start { density 1 H(0)  process disc() }\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpNamedStates `shouldBe` HM.empty

    describe "named state references in start blocks" $ do
        it "accepts density <Name> to reference a named state" $ do
            let src =
                    unlines
                        [ "state Bell := density 2 H(0).CNOT(0,1)"
                        , "start { density Bell  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    block <- onlyOf "start block" inp.inpStartBlocks
                    entry <- firstOf "start entry" block.sbEntries
                    entry.deRho.dim `shouldBe` 2

        it "fails when referencing an undeclared named state" $ do
            let src = "start { density Undefined  process disc() }\n"
            case parseFile src of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected failure for undeclared named state"

    describe "scoped tag-equiv blocks" $ do
        it "parses a scoped tag-equiv block" $ do
            let src =
                    unlines
                        [ "start \"honest\" { density 1 H(0)  process disc() }"
                        , "start \"adversary\" { density 1 H(0)  process disc() }"
                        , "tag-equiv \"honest\" \"adversary\" { 10 ~ 30 }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    (c1, c2, pairs) <- onlyOf "scoped tag-equiv block" inp.inpScopedTagEquiv
                    c1 `shouldBe` "honest"
                    c2 `shouldBe` "adversary"
                    pairs `shouldBe` [(ActionTag 10, ActionTag 30)]

        it "keeps scoped and global tag-equiv separate" $ do
            let src =
                    unlines
                        [ "start \"a\" { density 1 H(0)  process disc() }"
                        , "start \"b\" { density 1 H(0)  process disc() }"
                        , "tag-equiv { 1 ~ 2 }"
                        , "tag-equiv \"a\" \"b\" { 10 ~ 20 }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    inp.inpTagEquiv `shouldBe` [(ActionTag 1, ActionTag 2)]
                    length inp.inpScopedTagEquiv `shouldBe` 1

        it "parses scoped tag-equiv before start blocks" $ do
            let src =
                    unlines
                        [ "tag-equiv \"p\" \"q\" { 5 ~ 6 }"
                        , "start \"p\" { density 1 H(0)  process disc() }"
                        , "start \"q\" { density 1 H(0)  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    length inp.inpScopedTagEquiv `shouldBe` 1

        it "inpScopedTagEquiv is empty when no scoped block is present" $ do
            let src = "start { density 1 H(0)  process disc() }\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> inp.inpScopedTagEquiv `shouldBe` []

    describe "lookupStartBlock" $ do
        it "finds a named block by name" $ do
            let src =
                    unlines
                        [ "start \"alice\" { density 1 H(0)  process disc() }"
                        , "start \"bob\" { density 1 dm { 1,0; 0,0 }  process disc() }"
                        ]
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> do
                    sbName <$> lookupStartBlock "alice" inp `shouldBe` Just (Just "alice")
                    sbName <$> lookupStartBlock "bob" inp `shouldBe` Just (Just "bob")

        it "returns Nothing for a missing name" $ do
            let src = "start \"x\" { density 1 H(0)  process disc() }\n"
            case parseFile src of
                Left err -> expectationFailure err
                Right inp -> lookupStartBlock "missing" inp `shouldBe` Nothing
