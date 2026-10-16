{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative (execParser)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (AppendMode), stderr, withFile)

import Command.Check (checkCmd)
import Command.Equiv (equivCmd)
import Command.Eval (evalCmd)
import Command.Minimize (minimizeCmd)
import Command.Serve (serveCmd)
import Logging
import Options

withLogHandle :: Maybe FilePath -> (Handle -> IO a) -> IO a
withLogHandle Nothing k = k stderr
withLogHandle (Just path) k = withFile path AppendMode k

-- | Run a command; returns 'False' when it should exit with a failure status.
invoke :: (MonadLog m) => Command -> m Bool
invoke (Check opts) = checkCmd opts
invoke (Eval opts) = evalCmd opts
invoke (Minimize opts) = minimizeCmd opts
invoke (Equiv opts) = equivCmd opts
invoke (Serve opts) = True <$ liftIO (serveCmd opts)

main :: IO ()
main = do
    Invocation gopts cmd <- execParser commands
    let verbosity = severityFromFlags (goQuiet gopts) (goVerbose gopts)
    ok <- withLogHandle (goLogFile gopts) $ \h ->
        runAppWith (LogEnv verbosity h gopts.goTiming) $ invoke cmd
    unless ok exitFailure
