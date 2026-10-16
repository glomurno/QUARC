{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.ImportSpec (spec) where

import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import LqCCS.Import (loadQuarcFile, loadQuarcString, scanImportPaths)
import LqCCS.Parser.File (InputFileResult (..))
import LqCCS.Quantum (minimalMeasures, minimalOps)

spec :: Spec
spec = do
    describe "scanImportPaths" $ do
        it "returns empty list for content with no imports" $
            scanImportPaths "start { density 1 H(0) process disc() }" `shouldBe` []

        it "finds a single import path" $
            scanImportPaths "import \"lib/bell.quarc\"\nproc Foo() := disc()" `shouldBe` ["lib/bell.quarc"]

        it "finds multiple import paths in order" $
            scanImportPaths (unlines ["import \"a.quarc\"", "import \"b.quarc\"", "proc Foo() := disc()"]) `shouldBe` ["a.quarc", "b.quarc"]

        it "skips lines that are comments" $
            scanImportPaths "-- import \"ignored.quarc\"\nimport \"real.quarc\"" `shouldBe` ["real.quarc"]

        it "handles indented import lines" $
            scanImportPaths "  import \"indented.quarc\"" `shouldBe` ["indented.quarc"]

        it "ignores malformed import directives" $
            scanImportPaths "import not-a-string" `shouldBe` []

    describe "loadQuarcString (no imports)" $ do
        it "parses a file without imports and returns empty inpImports" $ do
            let src = "start { density 1 H(0)  process disc() }\n"
            result <- loadQuarcString "/tmp" src minimalOps minimalMeasures
            case result of
                Left err -> expectationFailure err
                Right inp -> do
                    inp.inpImports `shouldBe` []
                    length inp.inpStartBlocks `shouldBe` 1

    describe "loadQuarcFile (import resolution)" $ do
        it "resolves a proc declaration from an imported file" $
            withSystemTempDirectory "quarc-import-test" $ \dir -> do
                let libFile = dir </> "lib.quarc"
                    mainFile = dir </> "main.quarc"
                writeFile libFile "proc Lib() := disc()\n"
                writeFile mainFile $
                    unlines
                        [ "import \"lib.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
                result <- loadQuarcFile mainFile minimalOps minimalMeasures
                case result of
                    Left err -> expectationFailure err
                    Right inp -> do
                        inp.inpImports `shouldBe` []
                        length inp.inpDecls `shouldBe` 1

        it "resolves operator definitions from an imported file" $
            withSystemTempDirectory "quarc-import-test" $ \dir -> do
                let libFile = dir </> "ops.quarc"
                    mainFile = dir </> "main.quarc"
                writeFile libFile "operator MyH := adjoint(H)\n"
                writeFile mainFile $
                    unlines
                        [ "import \"ops.quarc\""
                        , "start { density 1 MyH(0)  process disc() }"
                        ]
                result <- loadQuarcFile mainFile minimalOps minimalMeasures
                case result of
                    Left err -> expectationFailure err
                    Right inp -> inp.inpImports `shouldBe` []

        it "resolves transitive imports (A imports B imports C)" $
            withSystemTempDirectory "quarc-import-test" $ \dir -> do
                let fileC = dir </> "c.quarc"
                    fileB = dir </> "b.quarc"
                    fileA = dir </> "a.quarc"
                writeFile fileC "proc C() := disc()\n"
                writeFile fileB $ unlines ["import \"c.quarc\"", "proc B() := disc()"]
                writeFile fileA $
                    unlines
                        [ "import \"b.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
                result <- loadQuarcFile fileA minimalOps minimalMeasures
                case result of
                    Left err -> expectationFailure err
                    Right inp ->
                        -- B directly declared; C transitively via B
                        length inp.inpDecls `shouldBe` 2

        it "reports an error for a missing import" $
            withSystemTempDirectory "quarc-import-test" $ \dir -> do
                let mainFile = dir </> "main.quarc"
                writeFile mainFile $
                    unlines
                        [ "import \"missing.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
                result <- loadQuarcFile mainFile minimalOps minimalMeasures
                case result of
                    Left err -> err `shouldContain` "missing.quarc"
                    Right _ -> expectationFailure "expected error for missing import"

        it "reports an import cycle" $
            withSystemTempDirectory "quarc-import-test" $ \dir -> do
                let fileA = dir </> "a.quarc"
                    fileB = dir </> "b.quarc"
                writeFile fileA $
                    unlines
                        [ "import \"b.quarc\""
                        , "start { density 1 H(0)  process disc() }"
                        ]
                writeFile fileB "import \"a.quarc\"\n"
                result <- loadQuarcFile fileA minimalOps minimalMeasures
                case result of
                    Left err -> err `shouldContain` "cycle"
                    Right _ -> expectationFailure "expected cycle error"
