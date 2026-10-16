{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Command.Serve
Description : The @serve@ subcommand — lqCCS stdio transport.

Reads newline-delimited JSON-RPC requests from stdin and writes responses to
stdout (one JSON object per line).

Each request runs in its own thread, so a long evaluation doesn't hold up the
editor's live @check@s, and a @cancel@ request can stop one by killing its
thread. While a request runs, @progress@ notifications report its phase.
Responses therefore arrive out of order; clients match them by @id@.
-}
module Command.Serve (serveCmd) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, evaluate, finally, fromException, try, uninterruptibleMask_)
import Control.Monad (forM_, unless)
import Data.Aeson (ToJSON, Value (Null), eitherDecodeStrict, encode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, isEOF, stdout)

import Options (ServeOptions (..))
import Server.Protocol (Progress (..), ReqMethod (..), Request (..), Response (..), Result (ROk), RpcError (..), appErrorCode, cancelledErrorCode, parseErrorCode)
import Server.Session (Session, handleWith, newSession)

-- | Transport state: the stdout lock and the in-flight requests by encoded id.
data Transport = Transport
    { outLock :: MVar ()
    , running :: MVar (Map BL.ByteString ThreadId)
    }

emit :: (ToJSON a) => Transport -> a -> IO ()
emit t msg = do
    let bytes = encode msg
    _ <- evaluate (BL.length bytes)
    uninterruptibleMask_ $ withMVar t.outLock $ \_ -> BLC.putStrLn bytes >> hFlush stdout

serveLoop :: Session -> Transport -> IO ()
serveLoop sess t = do
    eof <- isEOF
    unless eof $ do
        line <- BS.getLine
        unless (BS.null line) $
            case eitherDecodeStrict line of
                Left err -> emit t (Response Null (Left (RpcError parseErrorCode ("parse error: " ++ err) [])))
                Right (Request i (ReqCancel target)) -> do
                    m <- readMVar t.running
                    forM_ (Map.lookup (encode target) m) killThread
                    emit t (Response i (Right ROk))
                Right req -> spawn sess t req
        serveLoop sess t

-- | Run a request on its own thread, registered under its id for @cancel@.
spawn :: Session -> Transport -> Request -> IO ()
spawn sess t req@(Request i _) = do
    let key = encode i
    gate <- newEmptyMVar :: IO (MVar ())
    tid <- forkIO $ worker gate `finally` modifyMVar_ t.running (pure . Map.delete key)
    modifyMVar_ t.running (pure . Map.insert key tid)
    putMVar gate ()
  where
    report phase states = emit t (Progress i phase states)
    worker gate = do
        r <- try $ do
            () <- takeMVar gate
            resp <- handleWith report sess req
            emit t resp
        case r of
            Right () -> pure ()
            Left (e :: SomeException)
                | Just ThreadKilled <- fromException e ->
                    emit t (Response i (Left (RpcError cancelledErrorCode "cancelled" [])))
                | otherwise -> emit t (Response i (Left (RpcError appErrorCode ("internal error: " ++ show e) [])))

serveCmd :: ServeOptions -> IO ()
serveCmd _opts = do
    hSetBuffering stdout LineBuffering
    sess <- newSession
    t <- Transport <$> newMVar () <*> newMVar Map.empty
    serveLoop sess t
