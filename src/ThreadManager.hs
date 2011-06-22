{-# LANGUAGE DeriveDataTypeable, Rank2Types  #-}

-----------------------------------------------------------------------------
--
-- Module      :  ThreadManager
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module ThreadManager (
    ThreadManager,
    CrashBehaviour(..),
    ThreadInit,
    ThreadMain,
    ThreadSignal,
    ThreadCleanup,
    newThreadManager,
    stopThreadManager,
    spawnThread,
    exitThread
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception(SomeException, Exception, bracket, try, finally)
import Control.Monad (liftM2)
import Control.Monad.Error
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Either
import Data.Typeable
import System.Timeout
import qualified Data.Map as Map
import Test.HUnit
import WorkerTypes

import System.Log.Logger

-- | The action to take when a thread crashes - either restart the crashing thread, 
--   restart all threads or just ignore it and keep on trucking 
data CrashBehaviour = Ignore
                    | Restart
                    | RestartAll

type ThreadSignal = IO ()
type ThreadInit ts err = ErrorT err IO (ts, ThreadSignal)
type ThreadMain ts = ts -> IO ()
type ThreadCleanup ts = ts -> IO ()

data ThreadInfo ts err = ThreadInfo  {
  threadId      :: !ThreadId,
  threadInit    :: !(ThreadInit ts err),
  threadMain    :: !(ThreadMain ts),
  threadSignal  :: !ThreadSignal,
  threadCleanup :: !(ThreadCleanup ts),
  threadCrashBehaviour :: CrashBehaviour,
  exitActions :: ![ThreadInfo ts err -> IO ()]
}

data CtlMsg state err = NewThread (ThreadInit state err) 
                                  (ThreadMain state)
                                  (ThreadCleanup state) 
                                  CrashBehaviour
                                  (ThreadReply err)
                      | RestartThread ThreadId
                      | ExitThread ThreadId (ThreadReply err)
                      | ThreadExit ThreadId
                      | ThreadDeath ThreadId SomeException
                      | Shutdown (ThreadReply err)
                      | CheckThreadCount (ThreadReply err)

data CtlReply err = OK
                  | Thread ThreadId
                  | Error err

type CtlQ state err = TChan (CtlMsg state err)

type ThreadReply err = ReplyVar (CtlReply err)

type ThreadResult err a = Either err a
type ThreadResultIO err a = ErrorT err IO a

type ExitAction ts e = Error e => ThreadInfo ts e -> IO ()

-- | An external handle to a thread manager
data ThreadManager ts e = MkThreadMgr (CtlQ ts e)

data ThreadState = Running
                 | Restarting
                 | Exited

-- | The internal state of the thread manager
data TmState ts err = TmState {
  threads :: !(Map.Map ThreadId (ThreadInfo ts err))
}

newThreadManager :: (Error e) => IO (ThreadManager ts e)
newThreadManager = do
  debugLog "Creating new thread manager"
  ctlQ <- newTChanIO
  forkIO $ threadMgr ctlQ
  return $ MkThreadMgr ctlQ

-- | shuts down the thread manager and waits for any outstanding threads to
--   exit
stopThreadManager :: (ThreadManager ts e) -> IO ()
stopThreadManager (MkThreadMgr ctlQ) = do
  debugLog "Stoping thread manager"
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan ctlQ (Shutdown replyVar)

  debugLog "Waiting for thread manager to shut down"
  atomically $ takeTMVar replyVar

  debugLog "Thread manager stopped"
  return ()

-- | creates a new thread and waits for confirmation
spawnThread :: ThreadManager ts e -> 
               ThreadInit ts e ->
               ThreadMain ts ->
               ThreadCleanup ts -> 
               CrashBehaviour -> 
               IO (Either e ThreadId) 
spawnThread mgr init main cleanup crashBehaviour = do
  result <- do 
    replyVar <- spawnThreadWithoutWaiting mgr init main cleanup crashBehaviour
    atomically $ takeTMVar replyVar
  case result of
    Thread threadId -> return $ Right threadId
    Error err -> return $ Left err
    
-- | creates a new thread, without waiting for confirmation that the thread is 
--   alive
spawnThreadWithoutWaiting :: ThreadManager ts err -> 
                             ThreadInit ts err -> 
                             ThreadMain ts -> 
                             ThreadCleanup ts ->
                             CrashBehaviour -> IO (ThreadReply err)
spawnThreadWithoutWaiting (MkThreadMgr ctlQ) init main cleanup crashBehaviour = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan ctlQ (NewThread init main cleanup crashBehaviour replyVar)
  return replyVar
      
-- | Requests a thread gracefully exit and waits for it to happen
exitThread :: ThreadManager ts e -> ThreadId -> IO ()
exitThread (MkThreadMgr ctlQ) tid = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan ctlQ (ExitThread tid replyVar)
               
-- | The manager thread that looks after the pool of managed threads 
threadMgr :: (Error e) => CtlQ ts e -> IO ()
threadMgr ctlQ = loop ctlQ (TmState Map.empty)
  where
    loop :: (Error e) => CtlQ ts e -> TmState ts e -> IO ()
    loop ctlQ state = do
        msg <- atomically $ readTChan ctlQ
        case msg of
            NewThread init main cleanup restartBehaviour rpy -> do
              rval <- runErrorT $ forkThread init main cleanup restartBehaviour ctlQ state
              let msg    = either (\err -> Error err) (\(t,_) -> Thread t) rval
              let state' = either (\_ -> state) (\(_,x) -> x) rval
              atomically $ putTMVar rpy $! msg
              loop ctlQ $! state'
            
            ExitThread tid rpy -> do 
              state' <- requestThreadExit tid rpy state
              loop ctlQ state'
                          
            RestartThread tid -> do  
              state' <- onRestartThread tid ctlQ state
              loop ctlQ state'
              
            ThreadDeath tid err -> do
              debugLog $ "Detected ThreadDeath on Thread " ++ (show tid) ++ " with error" ++ (show err)
              state' <- onThreadDeath tid ctlQ state
              loop ctlQ state'
                
            ThreadExit tid -> do
              state' <- onThreadExit tid state
              loop ctlQ state'
              
            Shutdown rpy -> do
              state' <- onShutdownRequested ctlQ rpy state
              loop ctlQ state'
              
            CheckThreadCount rpy -> do
              let n = threadCount state
              debugLog $ (show n) ++ " pending threads"
              case n of
                0 -> atomically $ putTMVar rpy OK
                _ -> loop ctlQ state

threadCount :: TmState ts e -> Int
threadCount s = Map.size (threads s)

forkThread :: (Error err) => ThreadInit ts err -> 
              ThreadMain ts -> 
              ThreadCleanup ts -> 
              CrashBehaviour -> 
              CtlQ ts e -> 
              TmState ts err ->
              ErrorT err IO (ThreadId, TmState ts err)
forkThread init main cleanup crashBehaviour ctlQ state = do
    (threadState, sig) <- init
    tid <- liftIO $ forkIO $ threadWrapper main cleanup threadState ctlQ
    let threads' = Map.insert tid (ThreadInfo tid init main sig cleanup crashBehaviour []) (threads state)  
    let state' = state { threads = threads' }
    return (tid, state')
  where 
    threadWrapper :: ThreadMain ts -> ThreadCleanup ts -> ts -> CtlQ ts e -> IO ()
    threadWrapper main cleanup s ctlQ = do
      result <- try $ (main s) `finally` (cleanup s)
      tid <- myThreadId
      let msg = either (\err -> ThreadDeath tid err) (\_ -> ThreadExit tid) result
      debugLog $ "Sending exit notification to manager"
      atomically $ writeTChan ctlQ msg

-- | Requests that a managed thread exit, and appends an exit action to set the
--   supplied reply var when it does  
requestThreadExit :: Error e => ThreadId -> ThreadReply e -> TmState ts e -> IO (TmState ts e)
requestThreadExit tid rpy state = do
  case lookupThread tid state of
    Nothing   -> return state
    Just info -> do _ <- (threadSignal info)
                    let state' = addExitActionToThread tid (\_ -> atomically $ putTMVar rpy OK) state
                    return state'

-- | Adds an exit action to each managed thread in the thread map. More useful
--   than doing it all one-by-one in soe cases.
addExitActionToAll :: Error e => ExitAction ts e -> (TmState ts e) -> (TmState ts e)
addExitActionToAll action state = state { threads = threads' }
  where threads' = Map.map (addExitAction action) (threads state)
  
-- | Adds an exit actiom to a specific thread record, returning an updated
--   state info block 
addExitActionToThread :: Error e => ThreadId -> ExitAction ts e -> (TmState ts e) -> (TmState ts e)
addExitActionToThread tid action state = state { threads = threads' }
  where threads' = Map.adjust (addExitAction action) tid (threads state)
        
-- | Adds an exit action to a thread record, returning an updated thread record
addExitAction :: Error e => ExitAction ts e -> (ThreadInfo ts e) -> (ThreadInfo ts e)
addExitAction action info = info { exitActions = actions }
  where actions = action : (exitActions info)

-- | Updates the bookkeeping for the exited thread and runs the associated 
--   thread's exit actions
onThreadExit :: ThreadId -> TmState ts e -> IO (TmState ts e)
onThreadExit tid state = do
  debugLog $ "Detected ThreadExit on thread " ++ (show tid)
  case Map.lookup tid (threads state) of
    Nothing -> return state
    Just info -> do
      debugLog $ "Running " ++ ((show . length . exitActions) info) ++ " exit actions"
      mapM_ (\action -> action info) (exitActions info) 
      return $ forgetThread tid state
      
onThreadDeath :: Error e => ThreadId -> (CtlQ ts e) -> (TmState ts e ) -> IO (TmState ts e)
onThreadDeath tid ctlQ state = do
  case Map.lookup tid (threads state) of
    Nothing -> return state
    Just (ThreadInfo _ init main _ cleanup restartBehaviour _) -> do
      let state' = (forgetThread tid state)
      case restartBehaviour of
        Ignore -> 
          return state'
          
        Restart -> do
          rval <- runErrorT $ forkThread init main cleanup restartBehaviour ctlQ state'
          case rval of 
            Right (_, s) -> return s
            Left _ -> return state' {- panic panic panic - thread restart failed! -}
          
        RestartAll -> do 
          mapM_ (\tid -> atomically $ writeTChan ctlQ (RestartThread tid)) 
            $ Map.keys (threads state')
          return state'

-- | Handles a shutdown request by adding an exit action to all outstanding
--   threads and then throwing an ExitGracefully into them
--
--   Note that there may be issues if any threads have a pending restart action. 
onShutdownRequested :: Error e => CtlQ ts e -> ThreadReply e -> TmState ts e -> IO (TmState ts e)
onShutdownRequested ctlQ rpy state = do
  debugLog $ "Shutdown requested. Stopping all managed threads."
  mapM_ signalThreadExit $ Map.elems (threads state)
  atomically $ writeTChan ctlQ (CheckThreadCount rpy)
  return $ addExitActionToAll
             (\_ -> atomically $ writeTChan ctlQ (CheckThreadCount rpy)) state

-- | Restarts a thread by logging an exit action and raising an exception in 
--   the target thread 
onRestartThread :: Error e => ThreadId -> CtlQ ts e -> TmState ts e -> IO (TmState ts e)
onRestartThread tid ctlQ state = do
    case lookupThread tid state of
      Nothing   -> return state
      Just info -> return $ addExitActionToThread tid restartThread state
  where
    restartThread (ThreadInfo _ init main _ cleanup restartBehaviour _) = do
      reply <- newEmptyTMVarIO 
      atomically $ writeTChan ctlQ (NewThread init main cleanup restartBehaviour reply)

signalThreadExit :: ThreadInfo ts e -> IO ()
signalThreadExit info = threadSignal info
 
-- | Removes a thread record from the state record   
forgetThread :: ThreadId -> TmState ts e -> TmState ts e
forgetThread tid state = state { threads = Map.delete tid (threads state) }
  
lookupThread :: ThreadId -> TmState ts e -> Maybe (ThreadInfo ts e)
lookupThread tid state = Map.lookup tid (threads state)
 
-- ----------------------------------------------------------------------------
-- Debugging aids
-- ----------------------------------------------------------------------------
debugLog = debugM "thrdm" 

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------


unitTests = TestList []
