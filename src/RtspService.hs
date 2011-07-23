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
import Data.Char
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
import qualified Headers as Headers
import qualified Rtsp as Rtsp
import qualified Sdp as Sdp
import qualified Logger as Log
import ScriptExecutor
import Service

-- ----------------------------------------------------------------------------
-- RTSP Service Immplementation
-- ----------------------------------------------------------------------------

-- | "mutable" service state
data ServiceState = SvcState {
  svcRng          :: !PureMT,
  svcSessions     :: !SessionMap,
  svcAuthContexts :: !AuthContextMap,
  svcConnCount    :: !Int
}

type SessionId = Word64

type ConnectionId = Int

data RtspSession = Session {
  sessionId       :: !SessionId,
  snLastActivitiy :: !POSIXTime
}

type SessionVar = TVar RtspSession

type SessionMap = Map.Map SessionId SessionVar

type AuthContextMap = Map.Map ConnectionId AuthContext  

type StateVar = TVar ServiceState

type Realm = String

type ServerVersion = String

-- | A type alias that better reflects the use of the ScriptExecutor within the
--   context of the RTSP service. 
type Db = ScriptExecutor 

-- | A handle to an RTSP service. 
data RtspService = Svc {
  svcRealm        :: Realm,
  svcVersion      :: ServerVersion,
  svcState        :: StateVar,
  svcDb           :: Db
}

-- | Initialises the RTSP service 
newRtspService :: 
  Realm ->         -- ^ The realm that the server will report when authenticating a client 
  ServerVersion -> -- ^ The version string that the server will return on all responses
  Db ->            -- ^ The user and account database to be used for authentication
  IO RtspService
newRtspService realm version db = 
  let sessions = Map.empty
      contexts = Map.empty
  in do { rng <- newPureMT;
          svcVar <- newTVarIO $ SvcState rng sessions contexts 0;
          return $ Svc realm version svcVar db; }

-- | Registers a new connection with the server and spawns the threads that
--   will drive it. 
handleConnection :: 
  RtspService ->    -- ^ The RTSP Service that will "own" the new connection
  Int ->            -- ^ The amount of data the connectiuon will be allowed to buffer
  Socket ->         -- ^ The network socket representing the communications channel
  IO RtspConnection
handleConnection svc maxData s =
  let stateVar = svcState svc
  in do 
    connId <- atomically $ do state <- readTVar stateVar
                              let cid = 1 + (svcConnCount state)
                              writeTVar stateVar $! state {svcConnCount = cid}
                              return cid
    newConnection connId svc s maxData
  
-- | Acts on an RTSP request from a client
--
handleRequest :: RtspService -> RtspConnection -> (Rtsp.Message, Rtsp.MessageBody) -> IO ()
handleRequest svc conn (rq@(Rtsp.Request cseq method _ _ _), body) = do 
  forkIO $ do response <- case method of
                            "ANNOUNCE" -> handleAnnounce svc conn (rq, body)
                            _ -> return $ notImplemented cseq
              sendResponse conn response
  return () 
  
handleAnnounce :: RtspService -> RtspConnection -> (Rtsp.Message, Rtsp.MessageBody) -> IO (Rtsp.Message, Rtsp.MessageBody)
handleAnnounce rtsp conn (rq, body) = 
  let contentType = maybe "" (map toLower) $ Rtsp.msgGetHeaderValue rq "Content-Type"
      cseq = Rtsp.msgSequenceNumber rq
      sd = maybe Nothing Sdp.parse body
  in do
    if contentType /= "application/sdp" || sd == Nothing 
      then do debugLog $ "unsupported session description format " ++ contentType ++ " or malformed description" 
              return $ emptyResponse Rtsp.BadRequest cseq
      else withAuthenticatedUserDo rtsp conn rq $ \_ -> return (notImplemented cseq)
    
type AuthenticatedAction = UserId -> IO (Rtsp.Message, Rtsp.MessageBody)

-- | Handles the authentication of a request. If the user's authentication
--   checks out the fuction will apply the authenticated user to the supplied 
--   action. If the user doesn't check out the function will generate a 
--   request for authentication and return it to the client.
withAuthenticatedUserDo :: 
  RtspService  ->             -- ^ The current state of the RTSP server
  RtspConnection ->
  Rtsp.Message ->             -- ^ The RTSP request to authenticate
  AuthenticatedAction ->      -- ^ The action to run iff the user checks out
  IO (Rtsp.Message, Rtsp.MessageBody) -- ^ Returns an RTSP response to send to the client   
withAuthenticatedUserDo rtsp conn rq action =
  let realm = svcRealm rtsp
      cseq = Rtsp.msgSequence rq
      connId = connectionId conn 
  in do
    ctx <- getAuthContext rtsp connId
    userInfo <- runErrorT $ authenticate rtsp rq ctx
    debugLog $ "Authentication returned: " ++ (show userInfo)
    case userInfo of
      Right uid -> action uid
      Left err -> handleAuthFailure rtsp conn cseq ctx err

-- | Authentcates a user - i.e. checks that the user exists and that their
--   credentials match.
--
authenticate :: RtspService -> Rtsp.Message -> AuthContext -> AuthResultIO UserId
authenticate rtsp (Rtsp.Request _ verb uri _ hs) ctx = 
  let realm = svcRealm rtsp
      db = svcDb rtsp
  in do
    authResponse <- getAuthResponse
    userInfo <- lift . runMaybeT $ runScript $ queryUser db (authUser authResponse)
    case userInfo of 
      Nothing -> throwError NoSuchUser
      Just user -> do result <- ErrorT . return $ checkCreds verb authResponse user ctx
                      return $ fromUserInfo user
  where 
    getAuthResponse = case Headers.get "Authorization" hs >>= parseAuthHeader of
                             Just r -> return r
                             Nothing -> throwError MissingAuthorisation

-- | Generates an RTSP "authorization required" response, updating the
--   connection state with a new authentication context if need be.
--
handleAuthFailure :: 
  RtspService -> 
  RtspConnection ->
  Rtsp.SequenceNumber -> 
  AuthContext ->
  AuthFailure -> IO (Rtsp.Message, Rtsp.MessageBody)
handleAuthFailure rtsp@(Svc _ _ stateVar _) conn cseq ctx reason = do
    ctx' <- refreshAuthContext
    return $! authenticationRequired ctx'  
  where  
    refreshAuthContext = 
      if reason /= Stale 
        then return ctx
        else do now <- getPOSIXTime 
                let ctx' = Authentication.refreshAuthContext now ctx  
                atomically $ do state <- readTVar stateVar
                                let ctxs = svcAuthContexts state
                                let ctxs' = Map.alter (\_ -> Just ctx') (connectionId conn) ctxs
                                writeTVar stateVar $! state {svcAuthContexts = ctxs'} 
                return ctx'
  
    authenticationRequired :: AuthContext -> (Rtsp.Message, Rtsp.MessageBody)
    authenticationRequired authContext =
      let authHeaders = genAuthHeaders ctx reason
          hdrs = Headers.setValues Rtsp.hdrAuthenticate authHeaders Headers.empty
      in (Rtsp.Response cseq Rtsp.AuthorizationRequired hdrs, Nothing)

-- | Looks up an authentication context for a given connection and returns it
--   to the caller. If no authentication context is associated withe the 
--   connection then a new one is created, registered and then returned.
--  
getAuthContext :: RtspService -> ConnectionId -> IO AuthContext
getAuthContext rtsp@(Svc _ _ stateVar _) connId = 
  let realm = svcRealm rtsp
  in do now <- getPOSIXTime 
        atomically $ do state <- readTVar stateVar
                        let contexts = svcAuthContexts state 
                        case Map.lookup connId contexts of
                          Just ctx -> return ctx
                          Nothing -> let ctx = newAuthContext now realm 30
                                         contexts' = Map.insert connId ctx contexts
                                         state' = contexts' `seq` state {svcAuthContexts = contexts'}
                                      in do writeTVar stateVar $! state
                                            return ctx
                                            
badRequest :: (Rtsp.Message, Rtsp.MessageBody) 
badRequest = emptyResponse Rtsp.BadRequest 0

-- | Generates an otherwise-empty response 
notImplemented :: Rtsp.SequenceNumber -> (Rtsp.Message, Rtsp.MessageBody) 
notImplemented = emptyResponse Rtsp.NotImplemented

emptyResponse :: Rtsp.Status -> Rtsp.SequenceNumber -> (Rtsp.Message, Rtsp.MessageBody)  
emptyResponse status cseq = (Rtsp.Response cseq status Headers.empty, Nothing)
    
getMessageSession :: RtspService -> Rtsp.Message -> IO RtspSession
getMessageSession rtsp msg = let hdr = maybe "" id $ Rtsp.msgGetHeaderValue msg Rtsp.hdrSession
                                 sid = maybe 0 id $ parseSessionId hdr
                             in getOrNewSession rtsp sid
                             
parseSessionId :: String -> Maybe SessionId
parseSessionId s = case reads s of
                      [] -> Nothing
                      [(sid, _)] -> Just sid
    
getOrNewSession :: RtspService -> SessionId -> IO RtspSession
getOrNewSession (Svc _ _ stateVar _) sid = do
  now <- getPOSIXTime 
  atomically $ do state <- readTVar stateVar
                  let sessions = svcSessions state
                  case Map.lookup sid sessions of
                    Just sVar -> readTVar sVar
                    Nothing -> let (sid, rng') = randomWord64 $ svcRng state
                                   s = Session sid now
                               in do snVar <- newTVar $! s
                                     let sessions' = Map.insert sid snVar sessions
                                     writeTVar stateVar $! state {svcRng = rng', svcSessions = sessions'}
                                     return s
  
-- | Cr
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
  
data ItemToSend = MsgItem Rtsp.Message Rtsp.MessageBody
                | Packet Rtsp.Packet
                | QuitWriter

data ConnMsg = Hello ReplyVar
             | InboundRequest (Rtsp.Message, Rtsp.MessageBody)
             | OutboundResponse (Rtsp.Message, Rtsp.MessageBody)
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
  connId          :: !ConnectionId,
  connHandle      :: !RtspConnection,
  connSocket      :: !Socket,
  connState       :: !ConnState,
  connRtspService :: !RtspService,
  readerThread    :: !ThreadId,
  writerThread    :: !ThreadId,
  writerReplyVar  :: !(Maybe (TMVar ItemToSend)),
  sendQueue       :: ![ItemToSend],
  pendingMessages :: !(Map.Map Int Rtsp.Message)
}

type RtspSvc = Service ConnMsg ConnReply ConnInfo
  
data RtspConnection = MkConn ConnectionId RtspSvc

instance Show (TMVar a) where
  show _ = "tmvar"

instance Error Rtsp.Status where 
  noMsg    = Rtsp.InternalServerError
  strMsg _ = Rtsp.InternalServerError
  
connectionId :: RtspConnection -> ConnectionId
connectionId (MkConn connId _) = connId

-- | Spawns a new connection manager thread and returns an RtspConnection handle 
--   that can used to address the connection
newConnection :: ConnectionId -> RtspService -> Socket -> Int -> IO RtspConnection
newConnection connId rtsp s maxData = do
    svc <- newService (newState) (handleCall) (closeState)
    return $ MkConn connId svc
  where
    newState :: RtspSvc -> IO ConnInfo
    newState svc = 
      let realm = serviceRealm rtsp 
      in do debugLog "Entering new connection thread"
            readerTid <- forkIO $ reader s maxData svc
            writerTid <- forkIO $ writer s svc
            return $ State connId (MkConn connId svc) s ConnAlive rtsp readerTid writerTid Nothing [] Map.empty
      
    closeState :: ConnInfo -> IO ()
    closeState state = do 
      debugLog "Destroying connection"
      return ()
      
sendResponse :: RtspConnection -> (Rtsp.Message, Rtsp.MessageBody) -> IO ()
sendResponse (MkConn _ svc) msg = post svc $! OutboundResponse msg

-- | Handles a message from a client
handleCall :: ConnMsg -> ConnInfo -> IO (ConnReply, ConnInfo)
handleCall message state = 
  let conn = connHandle state
      rtsp = connRtspService state
  in do 
    case message of
      InboundRequest msg -> do debugLog $ "Received Msg: " ++ (show $ fst msg)
                               handleRequest rtsp conn msg
                               return (NoReply, state)
                                
      OutboundResponse msg  -> do state' <- sendOutboundMessage msg state
                                  return (NoReply, state')
                                        
      WriterExited -> do debugLog "Writer thread has exited"
                         sClose (connSocket state)
                         return (NoReply, state)
                                        
      InboundPacket pkt -> return (NoReply, state)
      
      GetItemToSend rx -> do state' <- getItemToSend rx state 
                             return (NoReply, state')
                             
      BadRequest -> do state' <- processBadRequest state 
                       return (NoReply, state')
                       
      _ -> do errorLog $ "Unexpected command message: " ++ (show message)
              return (NoReply, state)
     
sendOutboundMessage :: (Rtsp.Message, Rtsp.MessageBody) -> ConnInfo -> IO ConnInfo
sendOutboundMessage (msg, body) state = 
  let version = (serviceVersion . connRtspService) state
      msg' = Rtsp.msgSetHeaders msg [("Server", version)]
  in putItemToSend (MsgItem msg' body) state
  
processBadRequest :: ConnInfo -> IO ConnInfo
processBadRequest state = 
  let (msg, body) = badRequest
  in do state' <- putItemToSend (MsgItem msg body) (clearQueue state)
        disconnect state'
  
disconnect :: ConnInfo -> IO ConnInfo
disconnect state = do
  let sk     = connSocket state
  let state' = state {connState = ShuttingDown}
  shutdown sk ShutdownReceive
  putItemToSend QuitWriter state'

clearQueue :: ConnInfo -> ConnInfo
clearQueue state = state {sendQueue = []}

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
          post svc $! InboundRequest (msg, Just body)
          let state' = RS Nothing 0
          process svc remainder state'
          
  | (isNothing pending) && (B.head bytes == 0x24) = do
       case Rtsp.embeddedPacket bytes of 
         Just (packet, remainder) -> do 
           post svc $! InboundPacket packet
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
                else do post svc $! InboundRequest (msg, Nothing)
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