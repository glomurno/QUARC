module Command.ExitStatusSpec (spec) where

import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (stderr)
import Test.Hspec

import Command.Equiv (equivCmd)
import Command.Eval (evalCmd)
import Command.Minimize (minimizeCmd)
import Logging (App, LogEnv (..), Severity (Error), runAppWith)
import Options (EquivOptions (..), EquivSource (..), EvalOptions (..), Format (..), MinimizeOptions (..), ProcInput (..), ProcOutput (..))

import LqCCS.Bisimulation (Equivalence (..), Rooting (..), TagMode (..))
import LqCCS.Tagging (defaultTagPolicy)

run :: App a -> IO a
run = runAppWith (LogEnv Error stderr False)

evalOpts :: FilePath -> Maybe String -> EvalOptions
evalOpts path config = EvalOptions (InputFile path) StdOut [] Json config defaultTagPolicy

minimizeOpts :: FilePath -> MinimizeOptions
minimizeOpts path = MinimizeOptions (InputFile path) StdOut [] Json [] FileTags Strong mempty Nothing defaultTagPolicy

equivOpts :: EquivSource -> EquivOptions
equivOpts src = EquivOptions src StdOut [] [] FileTags Strong Unrooted mempty Json defaultTagPolicy

withQuarcFile :: String -> String -> (FilePath -> IO a) -> IO a
withQuarcFile name contents k = do
    tmp <- getTemporaryDirectory
    let path = tmp ++ "/quarc-exit-status-" ++ name ++ ".quarc"
    writeFile path contents
    r <- k path
    removeFile path
    pure r

missingImport :: String
missingImport = "import \"quarc-exit-status-does-not-exist.quarc\"\n"

spec :: Spec
spec = describe "CLI exit status" $ do
    it "eval fails on a missing input file" $
        run (evalCmd (evalOpts "quarc-exit-status-nope.quarc" Nothing)) `shouldReturn` False

    it "eval fails on a missing import" $
        withQuarcFile "eval-import" missingImport $ \p ->
            run (evalCmd (evalOpts p Nothing)) `shouldReturn` False

    it "eval fails when the requested start block does not exist" $
        withQuarcFile "eval-config" "" $ \p ->
            run (evalCmd (evalOpts p (Just "nosuch"))) `shouldReturn` False

    it "minimize fails on a missing import" $
        withQuarcFile "minimize-import" missingImport $ \p ->
            run (minimizeCmd (minimizeOpts p)) `shouldReturn` False

    it "equiv fails on a missing import" $
        withQuarcFile "equiv-import" missingImport $ \p ->
            run (equivCmd (equivOpts (NamedConfigs p "a" "b"))) `shouldReturn` False
