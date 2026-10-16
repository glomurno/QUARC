{-# LANGUAGE LambdaCase #-}

module OptionsSpec (spec) where

import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Test.Hspec

import Options

parse :: [String] -> Maybe Command
parse args = case execParserPure defaultPrefs commands args of
    Success (Invocation _ cmd) -> Just cmd
    _ -> Nothing

evalOutputOf :: [String] -> Maybe ProcOutput
evalOutputOf args =
    parse args >>= \case
        Eval o -> Just (evalOutput o)
        _ -> Nothing

minimizeOutputOf :: [String] -> Maybe ProcOutput
minimizeOutputOf args =
    parse args >>= \case
        Minimize o -> Just (minimizeOutput o)
        _ -> Nothing

equivOutputOf :: [String] -> Maybe ProcOutput
equivOutputOf args =
    parse args >>= \case
        Equiv o -> Just (equivOutput o)
        _ -> Nothing

spec :: Spec
spec = describe "output options" $ do
    it "names the default output after the input files and the command" $ do
        evalOutputOf ["eval", "dir/p.quarc"] `shouldBe` Just (OutputFile "p_eval")
        minimizeOutputOf ["minimize", "p.quarc"] `shouldBe` Just (OutputFile "p_minimize")
        equivOutputOf ["equiv", "a.quarc", "b.quarc"] `shouldBe` Just (OutputFile "a_b_equiv")
        equivOutputOf ["equiv", "p.quarc", "--config1", "x", "--config2", "y"] `shouldBe` Just (OutputFile "p_equiv")

    it "names the default output after the command alone for stdin" $
        evalOutputOf ["eval"] `shouldBe` Just (OutputFile "eval")

    it "uses the file stem given with -o" $ do
        evalOutputOf ["eval", "p.quarc", "-o", "out"] `shouldBe` Just (OutputFile "out")
        equivOutputOf ["equiv", "a.quarc", "b.quarc", "--output", "cmp"] `shouldBe` Just (OutputFile "cmp")

    it "writes to stdout only with --stdout" $ do
        evalOutputOf ["eval", "p.quarc", "--stdout"] `shouldBe` Just StdOut
        minimizeOutputOf ["minimize", "--stdout"] `shouldBe` Just StdOut

    it "rejects -o together with --stdout" $
        evalOutputOf ["eval", "p.quarc", "-o", "out", "--stdout"] `shouldBe` Nothing

    it "parses both equiv input modes" $ do
        equivSourceOf ["equiv", "a.quarc", "b.quarc"] `shouldBe` Just (TwoFiles "a.quarc" "b.quarc")
        equivSourceOf ["equiv", "a.quarc", "--config1", "x", "--config2", "y"] `shouldBe` Just (NamedConfigs "a.quarc" "x" "y")
  where
    equivSourceOf args =
        parse args >>= \case
            Equiv o -> Just (equivSource o)
            _ -> Nothing
