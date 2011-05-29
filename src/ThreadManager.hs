{-# LANGUAGE DeriveDataTypeable  #-}

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
    ExitGracefully,
    CrashBehaviour(..),
    newThreadManager,
    stopThreadManager,
    spawnThread,
    exitThread
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception(SomeException, Exception, bracket, try)
import Data.Either
import Data.Typeable
import System.Timeout
import qualified Data.Map as Map

-- | The action to take when a thread crashes - either restart the crashing thread, 
--   restart all threads or just ignore it and keep on trucking 
data CrashBehaviour = Ignore
                    | Restart
                    | RestartAll

-- | An exception thrown into the managed thread to signal a shutdown request
data ExitGracefully = ExitGracefully deriving (Show, Typeable)
instance Exception ExitGracefully

type ReplyVar = TMVar CtlReply
type ThreadMain = IO ()

data CtlMsg = NewThread ThreadMain CrashBehaviour ReplyVar
            | RestartThread ThreadId
            | ExitThread ThreadId ReplyVar
            | ThreadExit ThreadId
            | ThreadDeath ThreadId SomeException
            | Shutdown ReplyVar
            | CheckThreadCount ReplyVar

data CtlError = Timeout
              | Unexpected

data CtlReply = OK
              | Thread ThreadId
              | Error CtlError

type CtlQ = TChan CtlMsg

-- | An external handle to a thread manager
data ThreadManager = MkThreadMgr CtlQ

data ThreadState = Running
                 | Restarting
                 | Exited

data ThreadInfo = ThreadInfo  {
  threadMain :: ThreadMain,
  threadId :: ThreadId,
  threadCrashBehaviour :: CrashBehaviour,
  exitActions :: ![ThreadInfo -> IO ()]
}

-- | The internal state of the thread manager
data TmState = TmState {
  threads :: !(Map.Map ThreadId ThreadInfo)
}

newThreadManager :: IO ThreadManager
newThreadManager = do
    ctlQ <- newTChanIO
    forkIO $ threadMgr ctlQ
    return $ MkThreadMgr ctlQ

-- | shuts down the thread manager and waits for any outstanding threads to
--   exit
stopThreadManager :: ThreadManager -> IO ()
stopThreadManager (MkThreadMgr ctlQ) = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan ctlQ (Shutdown replyVar)
  atomically $ takeTMVar replyVar
  return ()

-- | creates a new thread and waits for confirmation
spawnThread :: ThreadManager -> ThreadMain -> CrashBehaviour -> IO (Either CtlError ThreadId)
spawnThread mgr main crashBehaviour = do
  replyVar <- spawnThreadWithoutWaiting mgr main crashBehaviour
  result <- atomically $ takeTMVar replyVar
  case result of
    Thread threadId -> return $ Right threadId
    Error err -> return $ Left err
    
-- | creates a new thread, without waiting for confirmation that the thread is 
--   alive
spawnThreadWithoutWaiting:: ThreadManager -> ThreadMain -> CrashBehaviour -> IO (ReplyVar)
spawnThreadWithoutWaiting (MkThreadMgr ctlQ) main crashBehaviour = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan ctlQ (NewThread main crashBehaviour replyVar)
  return replyVar
      
-- | Requests a thread gracefully exit and waits for it to happen
exitThread :: ThreadManager -> ThreadId -> IO ()
exitThread (MkThreadMgr ctlQ) tid = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan ctlQ (ExitThread tid replyVar)
               
-- | The manager thread that looks after the pool of managed threads 
threadMgr :: CtlQ -> IO ()
threadMgr ctlQ = do
    loop ctlQ (TmState Map.empty)
  where
    loop :: CtlQ -> TmState -> IO ()
    loop ctlQ state = do
        msg <- atomically $ readTChan ctlQ
        case msg of
            NewThread main restartBehaviour rpy -> do
              (tid, state') <- forkThread main restartBehaviour ctlQ state
              atomically $ putTMVar rpy (Thread tid)
              loop ctlQ state'
            
            ExitThread tid rpy -> do 
              state' <- requestThreadExit tid rpy state
              loop ctlQ state'
                          
            RestartThread tid -> do  
              state' <- onRestartThread tid ctlQ state
              loop ctlQ state'
              
            ThreadDeath tid err -> do
              state' <- onThreadDeath tid ctlQ state
              loop ctlQ state'
                
            ThreadExit tid -> do
              state' <- onThreadExit tid state
              loop ctlQ state'
              
            Shutdown rpy -> do
              state' <- onShutdownRequested ctlQ rpy state
              loop ctlQ state'
              
            CheckThreadCount rpy -> case threadCount state of
                                      0 -> atomically $ putTMVar rpy OK
                                      _ -> loop ctlQ state

threadCount :: TmState -> Int
threadCount s = Map.size (threads s)

forkThread :: ThreadMain -> CrashBehaviour -> CtlQ -> TmState -> IO (ThreadId, TmState)
forkThread main crashBehaviour ctlQ state = do
  tid <- forkIO $ threadWrapper main ctlQ
  let threads' = Map.insert tid (ThreadInfo main tid crashBehaviour []) (threads state)  
  let state' = state { threads = threads' }
  return (tid, state')
  
-- | Requests that a managed thread exit, and appends an exit action to set the
--   supplied reply var when it does  
requestThreadExit :: ThreadId -> ReplyVar -> TmState -> IO TmState
requestThreadExit tid rpy state = do
  throwTo tid ExitGracefully
  return $ addExitActionToThread tid (\_ -> atomically $ putTMVar rpy OK) state
  
-- | Adds an exit action to each managed thread in the thread map. More useful
--   than doing it all one-by-one in soe cases.
addExitActionToAll :: (ThreadInfo -> IO ()) -> TmState -> TmState
addExitActionToAll action state = state { threads = threads' }
  where threads' = Map.map (addExitAction action) (threads state)
  
-- | Adds an exit actiom to a specific thread record, returning an updated
--   state info block 
addExitActionToThread :: ThreadId -> (ThreadInfo -> IO ()) -> TmState -> TmState
addExitActionToThread tid action state = state { threads = threads' }
  where threads' = Map.adjust (addExitAction action) tid (threads state)
        
-- | Adds an exit action to a thread record, returning an updated thread record
addExitAction :: (ThreadInfo -> IO ()) -> ThreadInfo -> ThreadInfo
addExitAction action info = info { exitActions = actions }
  where actions = action : (exitActions info)

-- | Updates the bookkeeping for the exited thread and runs the associated 
--   thread's exit actions
onThreadExit :: ThreadId -> TmState -> IO TmState
onThreadExit tid state = do
    case Map.lookup tid (threads state) of
      Nothing -> return state
      Just info -> do
        mapM_ (\action -> action info) (exitActions info) 
        return $ forgetThread tid state
      
onThreadDeath :: ThreadId -> CtlQ -> TmState -> IO TmState
onThreadDeath tid ctlQ state = do
  case Map.lookup tid (threads state) of
    Nothing -> return state
    Just (ThreadInfo main _ restartBehaviour _) -> do
      let state' = (forgetThread tid state)
      case restartBehaviour of
        Ignore -> 
          return state'
          
        Restart -> do
          (_, s) <- forkThread main restartBehaviour ctlQ state'
          return s
          
        RestartAll -> do 
          mapM_ (\tid -> atomically $ writeTChan ctlQ (RestartThread tid)) 
            $ Map.keys (threads state')
          return state'

-- | Handles a shutdown request by adding an exit action to all outstanding
--   threads and then throwing an ExitGracefully into them
--
--   Note that there may be issues if any threads have a pending restart action. 
onShutdownRequested :: CtlQ -> ReplyVar -> TmState -> IO TmState
onShutdownRequested ctlQ rpy state = do
  mapM_ (\tid -> throwTo tid ExitGracefully) $ Map.keys (threads state)
  atomically $ writeTChan ctlQ (CheckThreadCount rpy)
  return $ addExitActionToAll
             (\_ -> atomically $ writeTChan ctlQ (CheckThreadCount rpy)) state

-- | Restarts a thread by logging an exit action and raising an exception in 
--   the target thread 
onRestartThread :: ThreadId -> CtlQ -> TmState -> IO TmState
onRestartThread tid ctlQ state = do 
    case Map.lookup tid (threads state) of
      Nothing -> return state
      Just info -> do
        let state' = addExitActionToThread tid restartThread state
        throwTo tid ExitGracefully
        return state'
  where
    restartThread :: ThreadInfo -> IO ()
    restartThread (ThreadInfo main _ restartBehaviour _) = do
      reply <- newEmptyTMVarIO 
      atomically $ writeTChan ctlQ (NewThread main restartBehaviour reply)
            
-- | Removes a thread record from the state record   
forgetThread :: ThreadId -> TmState -> TmState
forgetThread tid state = state { threads = Map.delete tid (threads state) }
   
-- | Wrapper for the execution of the supplied main action
threadWrapper :: ThreadMain -> CtlQ -> IO ()
threadWrapper main ctlQ = do
  result <- try main
  tid <- myThreadId
  let msg = case result of
              Right _ -> (ThreadExit tid)
              Left err -> (ThreadDeath tid err)
  atomically $ writeTChan ctlQ msg