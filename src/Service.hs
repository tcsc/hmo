module Service(
  Service,
  newService,
  stopService,
  call,
  post
) 
where 
  
import Control.Exception(SomeException, bracket, try)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import System.Timeout(timeout)
import System.Log.Logger

import WorkerTypes

data SvcMsg msg rpy = Call msg (ReplyVar rpy)
                    | Cast msg
                    | ExitSvc (ReplyVar ())

type SvcQ msg rpy = TChan (SvcMsg msg rpy)

data Service msg rpy state = Svc (SvcQ msg rpy)

-- | Defines a function that sets up a worker thread. Use this to initialise any
--   thread - specific data (e.g. database handles, etc) if necessary
type ServiceSetup msg rpy state = Service msg rpy state -> IO state

-- | Defines a function for tearing down resources created during thread startup
type ServiceTeardown state = state -> IO ()

-- | Defines a function type that the worker pool uses to handle messages
type ServiceHandler msg   -- ^ The range of possible messages
                    reply -- ^ The range of possible replies
                    state -- ^ An opaque per-thread state block for the owner
                    = msg -> state -> IO (reply, state)


-- | An erlang-style service framework

newService :: ServiceSetup msg rpy state -> 
              ServiceHandler msg rpy state -> 
              ServiceTeardown state -> IO (Service msg rpy state)
newService setup handler teardown = do
  q <- newTChanIO
  let svc = Svc q
  forkIO $ serverThread (setup svc) handler teardown q
  return svc
  
stopService :: Service msg rpy state -> IO ()
stopService (Svc q) = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan q (ExitSvc replyVar)
  atomically $ takeTMVar replyVar
  
call :: Service msg rpy state -> msg -> IO rpy
call (Svc q) msg = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan q (Call msg replyVar)   
  atomically $ takeTMVar replyVar
  
post :: Service msg rpy state -> msg -> IO ()
post (Svc q) msg = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan q (Cast msg)   
  
serverThread :: WorkerSetup s -> MessageHandler msg rpy s -> WorkerTeardown s -> SvcQ msg rpy -> IO ()
serverThread setup handler teardown msgq = bracket (setup) (teardown) (loop msgq handler)
  where
    loop :: SvcQ msg rpy -> MessageHandler msg rpy s -> s -> IO ()
    loop q hdlr state = do
      msg <- atomically $ readTChan q 
      case msg of
        ExitSvc rpyVar -> do 
          atomically $ putTMVar rpyVar ()
          return ()
          
        Cast msg -> do
          (_, state') <- hdlr msg state
          loop q hdlr state'
          
        Call msg rpyVar -> do
          (reply, state') <- hdlr msg state
          atomically $ putTMVar rpyVar reply 
          loop q hdlr state'
          
          