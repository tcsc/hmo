module Service(
  Service,
  newService,
  stopService,
  stopServiceWithoutWaiting,
  call,
  post,
  reply
) 
where 

-- | An erlang-style gen_service framework. Note that this service framework
--   imposes strict evaluation semantics on the service's state data, in that
--   the framework will force the evaluation of the service state after each
--   message is processed.
--
  
import Control.Exception(SomeException, bracket, try)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import System.Timeout(timeout)
import System.Log.Logger
import System.Timeout

import WorkerTypes

data SvcMsg msg = Request msg
                | ExitSvc (ReplyVar ())

type SvcQ msg = TChan (SvcMsg msg)

data Service msg state = Svc (SvcQ msg)

-- | Defines a function that sets up a worker thread. Use this to initialise any
--   thread - specific data (e.g. database handles, etc) if necessary
type ServiceSetup msg state = Service msg state -> IO state

-- | Defines a function for tearing down resources created during thread startup
type ServiceTeardown state = state -> IO ()

-- | Defines a function type that the worker pool uses to handle messages
type ServiceHandler msg   -- ^ The range of possible messages
                    state -- ^ An opaque per-thread state block for the owner
                    = msg -> state -> IO state

-- | Defines a famctory method that can take a TMVar and turn it in to a 
--   message. Used to automate the construction of messages that will return 
--   a value to the caller
type MsgFactory msg a = TMVar a -> msg  

-- | Creates a new service instance using the given setup, teardown and
--   processing functions.
newService :: ServiceSetup msg state -> 
              ServiceHandler msg state -> 
              ServiceTeardown state -> IO (Service msg state)
newService setup handler teardown = do
  q <- newTChanIO
  let svc = Svc q
  forkIO $ serverThread (setup svc) handler teardown q
  return svc  

stopService :: Service msg state -> IO ()
stopService svc = do
  replyVar <- stopServiceInternal svc
  atomically $ takeTMVar replyVar

stopServiceWithoutWaiting :: Service msg state -> IO ()
stopServiceWithoutWaiting svc = stopServiceInternal svc >> return ()
  
stopServiceInternal :: Service msg state -> IO (ReplyVar ())
stopServiceInternal (Svc q) = do 
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan q (ExitSvc replyVar)
  return replyVar

--  | Asks the service to perform some task and does not wait for a reply. Note
--    that the message value will be evaluated BEFORE being sent to the message
--    processing thread. 
post :: Service msg state -> msg -> IO ()
post (Svc q) msg = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan q (Request $! msg)   

-- | Asks the service to perform some task and waits for a reply. Note
--   that the message value will be evaluated BEFORE being sent to the message
--   processing thread.
call :: Service msg state -> MsgFactory msg a -> IO a
call (Svc q) f = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTChan q (Request $! f(replyVar))   
  atomically $ takeTMVar replyVar
  
callWithTimeout :: Service msg state -> MsgFactory msg a -> Int -> IO (Maybe a)
callWithTimeout svc f t = timeout t $ call svc f
  
  
reply :: TMVar a -> a -> IO ()
reply replyVar reply = atomically $ putTMVar replyVar $! reply

-- | Note that the server loop forces evaluation of the state data after each
--   message is processed.
serverThread :: WorkerSetup s -> ServiceHandler msg s -> WorkerTeardown s -> SvcQ msg -> IO ()
serverThread setup handler teardown msgq = bracket (setup) (teardown) (loop msgq handler)
  where
    loop :: SvcQ msg -> ServiceHandler msg s -> s -> IO ()
    loop q hdlr state = do
      msg <- atomically $ readTChan q 
      case msg of
        ExitSvc rpyVar -> do 
          atomically $ putTMVar rpyVar ()
          return ()
          
        Request msg -> do
          state' <- hdlr msg state
          loop q hdlr $! state'