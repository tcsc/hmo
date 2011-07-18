{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module RtspService(
  RtspService,
  RtspConnection,
  newRtspService,
  handleConnection,
  RtspService.unitTests
) where 
  
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception(Exception, catch, finally, bracket, try, throw)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Maybe
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Random.Mersenne.Pure64
import Test.HUnit(Test(..), assertBool, assertEqual)

import Authentication
import CommonTypes
import qualified Rtsp as Rtsp
import qualified Headers as Headers
import qualified Logger as Log
import ScriptExecutor
import Service

-- ----------------------------------------------------------------------------
-- RTSP Service Immplementation
-- ----------------------------------------------------------------------------

-- | "mutable" service state
data ServiceState = SvcState {
  svcRng :: !PureMT,
  svcSessions :: !SessionMap
}

type SessionId = Word64

data RtspSession = Session {
  sessionId       :: !SessionId,
  snLastActivitiy :: !POSIXTime
}

type SessionVar = TVar RtspSession

type SessionMap = Map.Map SessionId SessionVar

type StateVar = TVar ServiceState

type Realm = String

type ServerVersion = String

-- | A type alias that better reflects the use of the ScriptExecutor within the
--   context of the RTSP service. 
type Db = ScriptExecutor 

-- | A handle to an RTSP service. 
data RtspService = Svc Realm ServerVersion StateVar Db

-- | Initialises the RTSP service 
newRtspService :: 
  Realm ->         -- ^ The realm that the server will report when authenticating a client 
  ServerVersion -> -- ^ The version string that the server will return on all responses
  Db ->            -- ^ The user and account database to be used for authentication
  IO RtspService
newRtspService realm version db = 
  let sessions = Map.empty
  in do { rng <- newPureMT;
          svcVar <- newTVarIO $ SvcState rng sessions;
          return $ Svc realm version svcVar db; }

-- | Registers a new connection with the server and spawns the threads that
--   will drive it. 
handleConnection :: 
  RtspService ->    -- ^ The RTSP Service that will "own" the new connection
  Int ->            -- ^ The amount of data the connectiuon will be allowed to buffer
  Socket ->         -- ^ The network socket representing the communications channel
  IO RtspConnection
handleConnection svc maxData s = newConnection svc s maxData
  
newSession :: RtspService -> POSIXTime -> STM RtspSession
newSession (Svc _ _ stateVar _) now = do 
  state <- readTVar stateVar
  let (sid, rng') = randomWord64 $ svcRng state
  let sn = Session sid now
  snVar <- newTVar sn
  let svcSessions' = Map.insert sid snVar $ svcSessions state
  writeTVar stateVar $! state {svcRng = rng', svcSessions = svcSessions'}
  return sn
  
getSession :: RtspService -> SessionId -> STM (Maybe RtspSession)
getSession (Svc _ _ stateVar _) sid = do
  snVar <- readTVar stateVar >>= \state -> return $ Map.lookup sid $ svcSessions state
  case snVar of 
    Nothing -> return Nothing
    Just v -> readTVar v >>= \sn -> return $ Just sn 

genSessionId :: RtspService -> STM SessionId
genSessionId (Svc _ _ stateVar _) = do
  state <- readTVar stateVar
  let (sid, rng') = randomWord64 $ svcRng state
  writeTVar stateVar $! state {svcRng = rng'}
  return sid

serviceRealm :: RtspService -> Realm
serviceRealm (Svc r _ _ _) = r

serviceVersion :: RtspService -> ServerVersion
serviceVersion (Svc _ v _ _) = v

serviceDatabase :: RtspService -> Db
serviceDatabase (Svc _ _ _ db) = db

-- ----------------------------------------------------------------------------
-- RTSP Connection Implementation
-- ----------------------------------------------------------------------------

data ConnReply = NoReply
               | OK
               | Terminate
               deriving (Show)

type ReplyVar = TMVar ConnReply
  
data ItemToSend = MsgItem Rtsp.Message (Maybe B.ByteString)
                | Packet Rtsp.Packet
                | QuitWriter

data ConnMsg = Hello ReplyVar
             | InboundMsg Rtsp.Message (Maybe B.ByteString)
             | InboundPacket Rtsp.Packet
             | GetItemToSend (TMVar ItemToSend)
             | BadRequest
             | CloseConnection
             | WriterExited
             | ReaderExited
             deriving (Show)

data ConnState = ConnAlive
               | ShuttingDown
               deriving (Show, Eq)

-- | Defines a state block for the connection.
data ConnInfo = State {
  connSocket      :: !Socket,
  connState       :: !ConnState,
  connService     :: !RtspService,
  readerThread    :: !ThreadId,
  writerThread    :: !ThreadId,
  writerReplyVar  :: !(Maybe (TMVar ItemToSend)),
  sendQueue       :: ![ItemToSend],
  pendingMessages :: !(Map.Map Int Rtsp.Message),
  connAuthCtx     :: !AuthContext
}

type RtspSvc = Service ConnMsg ConnReply ConnInfo
  
data RtspConnection = MkConn RtspSvc

instance Show (TMVar a) where
  show _ = "tmvar"

instance Error Rtsp.Status where 
  noMsg    = Rtsp.InternalServerError
  strMsg _ = Rtsp.InternalServerError
  
-- | Spawns a new connection manager thread and returns an RtspConnection handle 
--   that can used to address the connection
newConnection :: RtspService -> Socket -> Int -> IO RtspConnection
newConnection rtsp s maxData = do
    svc <- newService (newState) (handleCall) (closeState)
    return $ MkConn svc
  where
    newState :: RtspSvc -> IO ConnInfo
    newState svc = 
      let realm = serviceRealm rtsp 
      in do debugLog "Entering new connection thread"
            readerTid <- forkIO $ reader s maxData svc
            writerTid <- forkIO $ writer s svc
            authCtx <- newAuthContext realm 30
            return $ State s ConnAlive rtsp readerTid writerTid Nothing [] Map.empty authCtx
      
    closeState :: ConnInfo -> IO ()
    closeState state = do 
      debugLog "Destroying connection"
      return ()

-- | Handles a message from a client
handleCall :: ConnMsg -> ConnInfo -> IO (ConnReply, ConnInfo)
handleCall (InboundMsg msg body) state = processMessage msg body state >>= \state' -> return (NoReply, state')
handleCall (InboundPacket pkt) state = return (NoReply, state)
handleCall (GetItemToSend rx) state = getItemToSend rx state >>= \state' -> return (NoReply, state')
handleCall BadRequest state = processBadRequest state >>= \state' -> return (NoReply, state')
handleCall WriterExited state = do
  debugLog "Writer thread has exited"
  sClose (connSocket state)
  return (NoReply, state)
  
handleCall _ state = return (NoReply, state)

-- | Handles the receipt of a message and begins processing it
processMessage :: Rtsp.Message -> Maybe B.ByteString -> ConnInfo -> IO ConnInfo
processMessage rq@(Rtsp.Request sq verb uri version headers) body state = do 
    debugLog $ "Received Msg: " ++ (show rq)
    (r, state') <- do case verb of 
                        "ANNOUNCE" -> handleAnnounce rq body state
                        _ -> notImplemented sq state
                     `catch` \(e :: ScriptError) -> return (internalServerError sq, state)
    sendResponse r Nothing state'
   
sendResponse :: Rtsp.Message -> Maybe B.ByteString -> ConnInfo -> IO ConnInfo
sendResponse msg body state = 
  let version = (serviceVersion . connService) state
      msg' = Rtsp.msgSetHeaders msg [("Server", version)]
  in putItemToSend (MsgItem msg' body) state

badRequest :: Rtsp.Message    
badRequest = Rtsp.Response 0 Rtsp.BadRequest Headers.empty

notImplemented :: Int -> ConnInfo -> IO (Rtsp.Message, ConnInfo) 
notImplemented cseq state = return $ (Rtsp.Response cseq Rtsp.NotImplemented Headers.empty, state)
  
processBadRequest :: ConnInfo -> IO ConnInfo
processBadRequest state = do
  state' <- putItemToSend (MsgItem badRequest Nothing) (clearQueue state)
  disconnect state'
  
disconnect :: ConnInfo -> IO ConnInfo
disconnect state = do
  let sk     = connSocket state
  let state' = state {connState = ShuttingDown}
  shutdown sk ShutdownReceive
  putItemToSend QuitWriter state'

clearQueue :: ConnInfo -> ConnInfo
clearQueue state = state {sendQueue = []}

-- | Handles an announcement request from an RTSP client.
--
handleAnnounce :: Rtsp.Message -> Maybe B.ByteString -> ConnInfo -> IO (Rtsp.Message, ConnInfo)
handleAnnounce rq@(Rtsp.Request cseq _ _ _ _) body state = 
  withAuthenticatedUserDo rq state $ \userInfo -> notImplemented cseq state

type AuthenticatedAction = UserId -> IO (Rtsp.Message, ConnInfo)

-- | Handles the authentication of a request. If the user's authentication
--   checks out the fuction will apply the authenticated user to the supplied 
--   action. If the user doesn't check out the function will generate a 
--   request for authentication and return it to the client.
withAuthenticatedUserDo :: 
  Rtsp.Message ->             -- ^ The RTSP request to authenticate
  ConnInfo ->                 -- ^ The current state of the connection
  AuthenticatedAction ->      -- ^ The action to run iff the user checks out
  IO (Rtsp.Message, ConnInfo) -- ^ Returns an RTSP response to send to the client 
                              --   and an updated connection state record.
withAuthenticatedUserDo rq state action = do
  userInfo <- runErrorT $ authenticate rq state
  debugLog $ "Authentication returned: " ++ (show userInfo)
  case userInfo of
    Right uid -> action uid
    Left Stale -> authRequired rq state
    Left MissingAuthorisation -> authRequired rq state
    Left BadCredentials -> authRequired rq state
    Left MalformedAuthRequest -> return (badRequest, state) 
    _ -> return (badRequest, state)

-- | Generates an RTSP "authorization required" response, updating the
--   connection state with a new authentication context if need be.
--
authRequired :: Rtsp.Message -> ConnInfo -> IO (Rtsp.Message, ConnInfo)
authRequired (Rtsp.Request cseq _ _ _ _) state = do
    (state', authCtx) <- getAuthContext state
    let (authCtx', hs) = genAuthHeaders authCtx
    let hdrs = foldl' setHeader Headers.empty hs  
    return $ (Rtsp.Response cseq Rtsp.AuthorizationRequired hdrs, state' {connAuthCtx=authCtx })
  where 
    setHeader :: Headers.Headers -> String -> Headers.Headers
    setHeader hs s = Headers.set "WWW-Authenticate" s hs
      
    getAuthContext :: ConnInfo -> IO (ConnInfo, AuthContext)
    getAuthContext s = let ctx = connAuthCtx s
                       in do isStale <- contextIsStale ctx
                             if not isStale then return (s, ctx)
                                            else do ctx' <- refreshAuthContext ctx
                                                    return (s {connAuthCtx = ctx'}, ctx')

-- | Lifts a "simple" maybe into a transformed MaybeT monad 
--
liftMaybe :: Monad m => Maybe a -> MaybeT m a 
liftMaybe = MaybeT . return

-- | Runs script action and translates the result into something that the
--   connection can understand.
--
runScript :: ScriptResultIO (Maybe a) -> MaybeT IO a
runScript action = do
  rval <- (lift . runErrorT) action
  case rval of 
    Right x -> liftMaybe x
    Left e -> throw e

-- | Authentcates a user - i.e. checks that the user exists and that their
--   credentials match.
--
authenticate :: Rtsp.Message -> ConnInfo -> AuthResultIO UserId
authenticate (Rtsp.Request _ verb uri _ hs) state = 
  let realm = (serviceRealm . connService) state
      db = (serviceDatabase . connService) state
      context  = connAuthCtx state
  in do
    response <- getResponse
    userInfo <- lift . runMaybeT $ runScript $ queryUser db (authUser response)
    case userInfo of 
      Nothing -> throwError NoSuchUser
      Just user -> do result <- ErrorT . return $ checkCreds verb response user context
                      return $ fromUserInfo user
  where 
    getResponse = case Headers.get "Authorization" hs >>= parseAuthHeader of
                    Just r -> return r
                    Nothing -> throwError MissingAuthorisation
    
    
-- | 
internalServerError :: Int -> Rtsp.Message
internalServerError sq = Rtsp.Response sq Rtsp.InternalServerError Headers.empty
 
-- | Puts an item in the send queue. Note that if the writer thread is waiting
--   on something to send, this function will wake it up and pass the supplied 
--   item directly to it, bypassing the queue.
-- 
--   This should only ever be called on the connection thread.
--
putItemToSend :: ItemToSend -> ConnInfo -> IO ConnInfo
putItemToSend item state = do 
  case writerReplyVar state of 
    Nothing -> do let q' = item : (sendQueue state) 
                  return state { sendQueue = q' } 
    Just rx -> do atomically $ putTMVar rx item 
                  return state { writerReplyVar = Nothing } 

-- | Pulls an item from the send queue and puts it in the supplied return 
--   envelope (a TMVar). If the send queue is empty, this function saves a
--   reference to the TMVar so that it can be filled when data arrives.
--
--   This should only ever be called on the connection management thread
getItemToSend :: TMVar ItemToSend -> ConnInfo -> IO ConnInfo
getItemToSend rx state = do
  case sendQueue state of 
    -- nothing to send, store the reply slot for filling later on
    [] -> return $ state { writerReplyVar = Just rx }
    h:t -> do atomically $ putTMVar rx h
              return $ state { sendQueue = t }
  
-- | The main writer thread. Asks the conection thread for data to send, 
--   formats it and then sends it out over the network.
writer :: Socket -> RtspSvc -> IO ()
writer s svc = do
    debugLog "Entering writer thread"
    receiver <- newEmptyTMVarIO
    writeLoop s svc receiver
    debugLog "Posting writer exiting message to connection thread"
    post svc WriterExited
    debugLog "Exiting writer thread"
  where
    writeLoop :: Socket -> RtspSvc -> (TMVar ItemToSend) -> IO ()
    writeLoop s svc rx = do
      post svc (GetItemToSend rx)
      item <- atomically $ takeTMVar rx
      case item of
        QuitWriter -> return () 
        _ -> do send s $ format item
                writeLoop s svc rx
      
    format :: ItemToSend -> B.ByteString
    format (MsgItem msg body) = Rtsp.formatMessage msg body
    format (Packet p) = Rtsp.formatPacket p
      
-- | The state block for the reader thread
data ReaderState = RS (Maybe Rtsp.Message) Int
  deriving (Show)
      
-- | The main reader loop. Reads data from the network, parses it and sends the
--   parsed messages on to the connection manager thread
reader :: Socket -> Int -> RtspSvc -> IO ()
reader s maxData svc = do 
    debugLog "Entering reader thread"
    readLoop s maxData (B.empty) svc (RS Nothing 0) `catch` 
      \(e :: IOError) -> do
        debugLog "Posting reader exited message" 
        post svc ReaderExited
    debugLog "Exiting reader thread"
  where 
    readLoop :: Socket -> Int -> B.ByteString -> RtspSvc -> ReaderState -> IO ()
    readLoop s maxData pending svc state = do
	    rdBuf <- recv s 512
	    let buffer = B.append pending $ rdBuf
	    (remainder, state') <- process svc buffer state
	    readLoop s maxData remainder svc state'
      
-- | Recursively pulls data out of the processing buffer and acts on it as 
--   necessary. Returns an updates state block and the remaining, unprocessed
--   bytes.
process :: RtspSvc -> B.ByteString -> ReaderState -> IO (B.ByteString, ReaderState)
process svc bytes state@(RS pending cLen)
  | B.length bytes == 0 = return (bytes, state)
  
  | isJust pending = do
      let msg = fromJust pending
      let contentLength = Rtsp.msgContentLength msg
      case (B.length bytes) >= contentLength of 
        False -> return (bytes, state)
        True -> do
          let (body, remainder) = B.splitAt contentLength bytes
          post svc (InboundMsg msg (Just body))
          let state' = RS Nothing 0
          process svc remainder state'
          
  | (isNothing pending) && (B.head bytes == 0x24) = do
       case Rtsp.embeddedPacket bytes of 
         Just (packet, remainder) -> do 
           post svc (InboundPacket packet)
           process svc remainder state
         Nothing -> return (bytes, state)
        
  | isNothing pending = do
      case  Rtsp.extractMessageBytes bytes of 
        Just (msg, remainder) -> do 
          case (Rtsp.parseMessage msg) of 
            Nothing -> do
              post svc BadRequest
              return (B.empty, state)

            Just msg -> do 
              let cLen' = Rtsp.msgContentLength msg
              if cLen' > 0 
                then process svc remainder (RS (Just msg) cLen)
                else do post svc (InboundMsg msg Nothing)
                        process svc remainder state
                        
        Nothing -> return (bytes, state)

-- ----------------------------------------------------------------------------
-- 
-- ----------------------------------------------------------------------------
errorLog :: String -> IO ()
errorLog = Log.err "rtspc"
 
debugLog :: String -> IO ()
debugLog = Log.debug "rtspc"

infoLog :: String -> IO ()
infoLog = Log.info "rtspc"

-- ----------------------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------------------

{-
testProcessMessage = TestCase (do 
  let b = Utf8.fromString "SETUP rtsp://localhost/root/1 RTSP/1.0\r\nCSeq: 2\r\n\r\n"
  let state = RS Nothing 0
  q <- newTChanIO
  (remainder, state') <- process q b state
  assertEqual "Remander Size" 0 (B.length remainder))

testProcessing = TestCase (do
  let bytes = B.concat [ Utf8.fromString "DESCRIBE rtsp://localhost/root/1 RTSP/1.0\r\nCSeq: 1\r\nContent-Length: 10\r\n\r\n",
                         B.pack [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                         B.pack [0x24, 16, 00, 05, 00, 01, 02, 03, 04],
                         Utf8.fromString "SETUP rtsp://localhost/root/1 RTSP/1.0\r\nCSeq: 2\r\n\r\n",
                         B.pack [0x24, 17, 00, 05, 00, 01, 02, 03, 04],
                         B.pack [0x24, 18, 00, 05, 00, 01, 02, 03, 04] ]
    
  q <- newTChanIO
  
  process q bytes (RS Nothing 0)
  
  m1 <- atomically $ readTChan q 
  case m1 of 
    InboundMsg msg (Just body) -> do
      assertEqual "Body Length" 10 (B.length body)
    _ -> do
      assertBool "Expected Message with body" False

  m2 <- atomically $ readTChan q 
  case m2 of
    InboundPacket (Rtsp.Packet ch p) -> do assertEqual "Channel" 16 ch
                                           assertEqual "Packet Size" 5 (B.length p)
    _ ->  assertBool "Expected Packet" False
    
  m3 <- atomically $ readTChan q 
  case m3 of
    InboundMsg msg Nothing -> return()
    _ -> assertBool "Expected message with no body" False) 
  
-}
--unitTests = TestList [testProcessing, testProcessMessage]
unitTests = TestList []