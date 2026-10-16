{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Minimal logging and phase-timing for the QUARC executable.

Commands run in 'App' (a thin @ReaderT LogEnv IO@) and emit messages
through 'MonadLog', which gates on a verbosity level and renders to either
stderr or a log file.
-}
module Logging (
    App,
    LogEnv (..),
    MonadLog (..),
    Severity (..),
    logAt,
    logDebug,
    logError,
    logInfo,
    logTrace,
    logWarn,
    runAppWith,
    severityFromFlags,
    withPhase,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTime)
import System.IO (Handle, hFlush)
import Text.Printf (printf)

data Severity = Error | Warn | Info | Debug | Trace
    deriving (Eq, Ord, Show, Enum, Bounded)

data LogEnv = LogEnv
    { leVerbosity :: !Severity
    -- ^ Highest severity that will be emitted.
    , leHandle :: !Handle
    -- ^ Sink for log lines (stderr or a file).
    , leTiming :: !Bool
    -- ^ When set, 'withPhase' always reports wall-clock timings.
    }

newtype App a = App (ReaderT LogEnv IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader LogEnv)

runAppWith :: LogEnv -> App a -> IO a
runAppWith env (App m) = runReaderT m env

class (MonadIO m) => MonadLog m where
    askLogEnv :: m LogEnv

instance MonadLog App where
    askLogEnv = ask

{- | Derive a verbosity from the CLI flags: @--quiet@ forces 'Error' only,
otherwise the default is 'Warn' raised one level per @-v@ (clamped to 'Trace').
-}
severityFromFlags :: Bool -> Int -> Severity
severityFromFlags True _ = Error
severityFromFlags False n = toEnum (min (fromEnum (maxBound @Severity)) (fromEnum Warn + max 0 n))

prefix :: Severity -> Text
prefix Error = "[error] "
prefix Warn = "[warn ] "
prefix Info = "[info ] "
prefix Debug = "[debug] "
prefix Trace = "[trace] "

logAt :: (MonadLog m) => Severity -> Text -> m ()
logAt sev msg = do
    env <- askLogEnv
    when (sev <= leVerbosity env) $ liftIO $ do
        TIO.hPutStrLn (leHandle env) (prefix sev <> msg)
        hFlush (leHandle env)

logError, logWarn, logInfo, logDebug, logTrace :: (MonadLog m) => Text -> m ()
logError = logAt Error
logWarn = logAt Warn
logInfo = logAt Info
logDebug = logAt Debug
logTrace = logAt Trace

{- | Run an action, measuring its wall-clock duration. The duration is reported
at 'Debug' level, or unconditionally on a @[timing]@ line when @--timing@ is
set.

Note: Due to laziness, this only times forced work. Heavy pure phases (e.g.
building the LTS) should force a summary (e.g. the state count) inside the
action so the timing is meaningful.
-}
withPhase :: (MonadLog m) => Text -> m a -> m a
withPhase name act = do
    env <- askLogEnv
    logTrace ("→ " <> name)
    start <- liftIO getMonotonicTime
    r <- act
    end <- liftIO getMonotonicTime
    let ms = (end - start) * 1000 :: Double
        line = name <> " took " <> T.pack (printf "%.2f ms" ms)
    if leTiming env
        then liftIO $ do
            TIO.hPutStrLn (leHandle env) ("[timing] " <> line)
            hFlush (leHandle env)
        else logDebug ("✓ " <> line)
    pure r
