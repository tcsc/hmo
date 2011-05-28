-----------------------------------------------------------------------------
--
-- Module      :  WorkerPool
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Defines a mechanism for implementing erlang-style multiprocessing.
--   Spawns a pool of worker threads and a manager thread to oversee them.
--   The pool is designed to execute multiple, short-lived tasks. For a set
--   of long-running tasks, you might want to use a ThreadManager
-----------------------------------------------------------------------------

module WorkerPool(WorkerPool,
                  WorkerSetup,
                  WorkerTeardown,
                  MessageHandler,
                  MessageReply (..),
                  call,
                  callWithTimeout,
                  post,
                  newWorkerPool,
                  stopWorkerPool) where

import Control.Exception(SomeException, bracket, try)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import System.Timeout(timeout)
import System.Log.Logger

import WorkerTypes

-- | Defines a set of messages used for iternal communication between the
--   Manager thread and the worker threads
data MgrMsg = Exit
            | ThreadExit ThreadId (Maybe SomeException)

type MgrQ = TChan MgrMsg

-- | Defines a function that sets up a worker thread. Use this to initialise any
--   thread - specific data (e.g. database handles, etc) if necessary
type WorkerSetup state = IO state

-- | Defines a function for tearing down resources created during thread startup
type WorkerTeardown state = state -> IO ()

-- | Defines a function type that the worker pool uses to handle messages
type MessageHandler msg   -- ^ The range of possible messages
                    reply -- ^ The range of possible replies
                    state -- ^ An opaque per-thread state block for the owner
                    = msg -> state -> IO (MessageReply reply, state)

-- | A collection of callback functions used to manage setup, teardown and
--   drive the worker process
data PoolFuns msg reply threadState =
  PoolFuns (WorkerSetup threadState)
           (WorkerTeardown threadState)
           (MessageHandler msg reply threadState)

-- | A handle for a worker pool that external modules can use
data WorkerPool msg reply = MkWorkerPool MgrQ (WkrQ msg reply)

-- | The internal state structure for the worker pool
data PoolState msg reply state = PoolState { poolThreadCount :: Int,
                                             poolThreads :: [ThreadId],
                                             poolFuns :: PoolFuns msg reply state }

-- | Error conditions for dealing with workers
data WorkerPoolError = Timeout

-- | A shorthand representation of a reply from a worker pool thread.
type WorkerResultIO reply = IO (Either WorkerPoolError reply)

-- | Forks off a pool manager thread and returns a handle to the newly created
--   worker pool
newWorkerPool :: Int ->
                 WorkerSetup state ->              -- ^ creates a new worker thread state
                 WorkerTeardown state ->           -- ^ destroys a worker thread state
                 MessageHandler msg reply state -> -- ^ the message handling function
                 IO (WorkerPool msg reply)
newWorkerPool threads setup teardown handler = do
  mgrQ <- newTChanIO
  wkrQ <- newTChanIO
  let funs = PoolFuns setup teardown handler
  _ <- forkIO $ mgrThreadMain threads mgrQ wkrQ funs
  return $ MkWorkerPool mgrQ wkrQ

-- | Stops the worker pool, but does not wait for the pool threads to exit.
stopWorkerPool :: WorkerPool msg reply -> IO ()
stopWorkerPool (MkWorkerPool mgrQ _) = do
  atomically $ writeTChan mgrQ Exit

-- | The main routine for the pool manager thread. Starts and stops the worker
--   threads as well as monitring the management queue for messages and acts on
--   them.
mgrThreadMain :: Int -> MgrQ -> WkrQ msg reply -> PoolFuns msg reply state -> IO ()
mgrThreadMain nThreads mgrQ wkrQ handlers = do
    ts <- mapM (\_ -> forkWorker mgrQ wkrQ handlers ) [1..nThreads]
    loop mgrQ wkrQ $ PoolState { poolThreadCount = nThreads,
                                 poolThreads = ts,
                                 poolFuns = handlers }
  where
    loop :: MgrQ -> WkrQ msg reply -> PoolState msg reply state -> IO ()
    loop mgrQ wkrQ state = do
      msg <- atomically $ readTChan mgrQ
      case msg of
        Exit -> do
          atomically $ mapM (\_ -> writeTChan wkrQ ExitWorker) [1..nThreads]
          loop mgrQ wkrQ state

        ThreadExit tid maybeError -> do
          let tids = delete tid (poolThreads state)
          case maybeError of
            -- The thread has exited normally, probably as part of a controlled
            -- shutdown. Let it go.
            Nothing -> loop mgrQ wkrQ $ state { poolThreads = tids }

            -- The thread has crashed. Restart it and hope that whatever caused
            -- the crash has gone away.
            Just e -> do
              logError $ (show tid) ++ " crashed with " ++ (show e)
              newTid <- forkWorker mgrQ wkrQ (poolFuns state)
              loop mgrQ wkrQ $ state { poolThreads = newTid : tids }

-- | Forks off a worker thread that will handle messages from the worker queue
--   and process them, posting a message back to the manager thread when it
--   exits for whatever reason
forkWorker :: MgrQ -> WkrQ msg reply -> PoolFuns msg reply state -> IO ThreadId
forkWorker mgrQ wkrQ funs = forkIO $ tryWorker mgrQ wkrQ funs
  where
    tryWorker :: MgrQ -> WkrQ msg reply-> PoolFuns msg reply state -> IO ()
    tryWorker mgrQ wkrQ (PoolFuns setup teardown handler) = do
      tid <- myThreadId
      result <- try $ bracket setup teardown (loop mgrQ wkrQ handler)
      let response = either (Just) (\_ -> Nothing) result
      atomically $ writeTChan mgrQ $ ThreadExit tid response

    loop :: MgrQ -> WkrQ msg reply-> MessageHandler msg reply state -> state -> IO ()
    loop mgrQ wkrQ handler state = do
      msg <- atomically $ readTChan wkrQ
      case msg of
        Handle m -> do
          (_, state') <- handler m state
          loop mgrQ wkrQ handler state'
        HandleAndReply m r -> do
          (reply, state') <- handler m state
          sendReply r reply
          loop mgrQ wkrQ handler state'

-- | Posts a message to the worker pool and awaits a reply
call :: WorkerPool msg reply -> msg -> IO reply
call (MkWorkerPool _ wkrQ) msg = do
  replyVar <- newReplyVar
  atomically $ writeTChan wkrQ $! HandleAndReply msg replyVar
  atomically $ takeTMVar replyVar

-- | Posts a message to the worker queue and waits - up to a given timeout - for
--   a reply. Returns Nothing if the request times out.
callWithTimeout :: WorkerPool msg reply -> msg -> Int -> WorkerResultIO reply
callWithTimeout (MkWorkerPool _ wkrQ) msg t = do
  replyVar <- newReplyVar
  atomically $ writeTChan wkrQ $! HandleAndReply msg replyVar
  result <- timeout t $ atomically $ takeTMVar replyVar
  case result of
    Just rpy -> return $ Right rpy
    Nothing -> return $ Left Timeout

-- | Posts a message to the worker pool and does not await a reply
post :: WorkerPool msg reply -> msg -> IO ()
post (MkWorkerPool _ wkrQ) msg = atomically $
  writeTChan wkrQ $! Handle msg

-- | Forces the evaluation of the reply and sends it back to the caller via
--   the supplied ReplyVar
sendReply :: ReplyVar reply -> MessageReply reply -> IO ()
sendReply replyVar reply = do 
  case reply of 
    NoReply -> return ()
    Reply r -> atomically $ putTMVar replyVar $! r

newReplyVar :: IO (ReplyVar reply)
newReplyVar = newEmptyTMVarIO

debug = debugM "pool"
logError = errorM "pool"
