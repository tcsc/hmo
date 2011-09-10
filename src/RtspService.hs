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
import Data.Int
import Network.URI
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
import RtpTransport
import RtpReceiver
import ScriptExecutor
import SessionManager
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

type SessionId = Int64

type ConnectionId = Int

-- | State data for each sesison
data RtspSession = Session {
  sessionId           :: !SessionId,
  sessionActivitiy    :: !POSIXTime,
  sessionReceivers    :: ![RtpReceiver]
}

type SessionVar = TVar RtspSession

type SessionMap = Map.Map SessionId SessionVar

type AuthContextMap = Map.Map ConnectionId AuthContext  

type StateVar = TVar ServiceState

type Realm = String

type ServerVersion = String

-- | An RTSP error type to help with sequencing actions that can fail
type RtspResultIO = ErrorT Rtsp.Status IO

-- | A type alias that better reflects the use of the ScriptExecutor within the
--   context of the RTSP service. 
type Db = ScriptExecutor 

-- | A handle to an RTSP service. 
data RtspService = Svc {
  svcRealm        :: Realm,
  svcVersion      :: ServerVersion,
  svcState        :: StateVar,
  svcDb           :: Db,
  svcSessionMgr   :: SessionManager
}

-- | Initialises the RTSP service 
newRtspService :: 
  Realm ->          -- ^ The realm that the server will report when authenticating a client 
  ServerVersion ->  -- ^ The version string that the server will return on all responses
  Db ->             -- ^ The user and account database to be used for authentication
  SessionManager -> -- ^  
  IO RtspService
newRtspService realm version db sMgr = 
  let sessions = Map.empty
      contexts = Map.empty
  in do { rng <- newPureMT;
          svcVar <- newTVarIO $ SvcState rng sessions contexts 0;
          return $ Svc realm version svcVar db sMgr; }

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
handleRequest :: 
  RtspService -> 
  RtspConnection -> 
  Rtsp.Message -> 
  IO ()
handleRequest svc conn msg@(rq@(Rtsp.Request cseq method _ _ _), body) = do 
  forkIO $ do response <- case method of
                            "OPTIONS" -> handleOptions cseq
                            "ANNOUNCE" -> handleAnnounce svc conn msg
                            "SETUP" -> handleSetup svc conn msg
                            _ -> return $ notImplemented cseq
              sendResponse conn response
  return () 
  
-- | Performs the supplied action and, depending on the result, either executes
--   the supplied action or generates an appropriate RTSP error response. 
--   
withSessionResultDo :: 
  RtspService -> 
  RtspConnection -> 
  Rtsp.Message -> 
  SessionResultIO a ->      -- ^ 
  (a -> IO Rtsp.Message) -> -- ^
  IO Rtsp.Message
withSessionResultDo rtsp conn (rq, _) result action = 
  let cseq = Rtsp.msgSequence rq
      connId = connectionId conn
  in do
    result <- runErrorT $ result 
    case result of 
      Right val -> action val
      Left err -> case err of
                    NotFound -> return $ emptyResponse Rtsp.NotFound cseq
                    Unauthorised -> do ctx <- getAuthContext rtsp connId
                                       return $ handleAuthFailure cseq ctx Unauthorized

handleOptions :: Rtsp.SequenceNumber -> IO Rtsp.Message
handleOptions cseq = 
  let options = "ANNOUNCE, DESCRIBE, SETUP, PLAY, TEARDOWN, RECORD"
      hs = Headers.fromList [(Rtsp.hdrPublic, options)]
      response = Rtsp.Response cseq Rtsp.OK hs
  in return (response, Nothing)

-- | Handles an RTSP announce request by trying to create a new session at the
--   requested path.
handleAnnounce :: 
  RtspService ->    -- ^ The RTSP service
  RtspConnection -> -- ^ The RTSP connection that wants to create a session
  Rtsp.Message ->   -- ^ The RTSP request describing the session to create
  IO (Rtsp.Message)
handleAnnounce rtsp conn msg@(rq, body) = 
  let contentType = maybe "" (map toLower) $ Rtsp.msgGetHeaderValue rq Rtsp.hdrContentType
      cseq = Rtsp.msgSequenceNumber rq
      sd = maybe Nothing Sdp.parse body
      sMgr = svcSessionMgr rtsp
      path = uriPath $ Rtsp.reqURI rq
      withValidUserDo = withAuthenticatedUserDo rtsp conn rq
      bind = withSessionResultDo rtsp conn msg
  in do
    if contentType /= "application/sdp" || sd == Nothing 
      then return $ emptyResponse Rtsp.BadRequest cseq
      else withValidUserDo $ \userId -> let desc = fromJust sd
                                            s = createSession sMgr path desc userId
                                        in do debugLog $ "Creating session at " ++ path
                                              s `bind` \_ -> do debugLog "Session created"
                                                                return $ ok cseq

-- | Handles a SETUP request for an individual stream
handleSetup ::
  RtspService ->
  RtspConnection ->
  Rtsp.Message ->
  IO (Rtsp.Message)
handleSetup rtsp@(Svc _ _ stateVar _ _) conn msg@(rq, body) = 
  let cseq = Rtsp.msgSequenceNumber rq
      sMgr = svcSessionMgr rtsp
      path = uriPath $ Rtsp.reqURI rq
      hdr = maybe "" id $ Rtsp.msgGetHeaderValue rq Rtsp.hdrTransport
      transportSpec = Rtsp.parseTransport hdr
      withMaybeValidUserDo = withMaybeAuthenticatedUserDo rtsp conn rq
  in do 
    case transportSpec of 
      Nothing -> return $ emptyResponse Rtsp.BadRequest cseq
      Just t -> do 
        rtspSession <- getMessageSession rtsp rq
        withMaybeValidUserDo $ \maybeUserId -> do 
          x <- runErrorT $ runSetup path rtspSession t maybeUserId
          case x of
            Right transport -> let hs = Headers.fromList [
                                          (Rtsp.hdrTransport, show $ transportGetSpec transport), 
                                          (Rtsp.hdrSession, show $ sessionId rtspSession)]
                               in return $ (Rtsp.Response cseq Rtsp.OK hs, Nothing)
            Left s -> return $ emptyResponse s cseq
               
  where
    runSetup :: String -> RtspSession -> TransportSpec -> (Maybe UserId) -> RtspResultIO RtpTransport
    runSetup path session clientSpec user = 
      let mode = \t -> maybe Play (\(Mode m) -> m) $ find isModeParameter $ transportParams t
      in do debugL $ "Creating new RTP transport for " ++ (show clientSpec) 
            transport <- newRtpTransport clientSpec conn
            session' <- case mode clientSpec of 
                          Record -> registerSource path transport session user
                          -- Play -> 
            liftIO $ updateSession rtsp session'
            return transport
            
    registerSource :: String -> RtpTransport -> RtspSession -> (Maybe UserId) -> RtspResultIO RtspSession 
    registerSource path transport session user = 
      let sMgr = svcSessionMgr rtsp
      in do rval <- liftIO $ do (rx, s') <- createRtpReceiver transport session
                                src <- getChunkSource rx
                                x <- runErrorT $ bindStreamSource sMgr path src user
                                case x of 
                                  Right _ -> return $ Right s'
                                  Left e -> do destroyRtpReceiver rx
                                               return $ Left e
            case rval of 
              Right s' -> return s'
              Left e -> throwError (translateSessionError e)         
                                                        
    createRtpReceiver :: RtpTransport -> RtspSession -> IO (RtpReceiver, RtspSession)
    createRtpReceiver transport session = 
      let rxs = sessionReceivers session 
      in do rx <- newRtpReceiver transport 
            let session' = session { sessionReceivers = rx : rxs }
            return (rx, session')
            
translateSessionError :: SessionError -> Rtsp.Status
translateSessionError err = 
  case err of 
    Unauthorised -> Rtsp.AuthorizationRequired
    _ -> Rtsp.InternalServerError
              
newRtpTransport :: TransportSpec -> RtspConnection -> RtspResultIO RtpTransport
newRtpTransport t conn = 
  let protocol = transportLowerTransport t
  in do case protocol of
          TCP -> newRtspTransport conn t
          _ -> throwError Rtsp.UnsupportedTransport
      
-- | Creates an RTP receiver (an RTP receiver translates RTP packets into
--   chunks that the stream can use) 
--createRtpReceiver conn transport session = 
      
-- | An action that will be executed if a request is properly
--   authenticated.
type AuthenticatedAction = UserId -> IO Rtsp.Message

-- | Handles the authentication of a request. If the user's authentication
--   checks out the fuction will apply the authenticated user to the supplied 
--   action. If the user doesn't check out the function will generate a 
--   request for authentication and return it to the client.
withAuthenticatedUserDo :: 
  RtspService  ->        -- ^ The current state of the RTSP server
  RtspConnection ->      -- ^ The connection that we're trying to authenticate
  Rtsp.MessageHeader ->  -- ^ The RTSP request to authenticate
  AuthenticatedAction -> -- ^ The action to run iff the user checks out
  IO Rtsp.Message        -- ^ Returns an RTSP response to send to the client   
withAuthenticatedUserDo rtsp conn rq action = 
  let cseq = Rtsp.msgSequence rq 
      connId = connectionId conn 
  in do
    ctx <- getAuthContext rtsp connId 
    withAuthenticationResultDo rtsp conn ctx rq $ \ar -> 
      case ar of
        Right uid -> action uid
        Left err  -> return $ handleAuthFailure cseq ctx err

-- | Handles the authentication of a request, but does *not* automatically
--   fail the transaction if there is no authentication header.  
withMaybeAuthenticatedUserDo ::
  RtspService  ->                      -- ^ The current state of the RTSP server
  RtspConnection ->                    -- ^ The connection that we're trying to authenticate
  Rtsp.MessageHeader ->                -- ^ The RTSP request to authenticate
  (Maybe UserId -> IO Rtsp.Message) -> -- ^ The action to run if the user checks out
  IO Rtsp.Message                      -- ^ Returns an RTSP response to send to the client   
withMaybeAuthenticatedUserDo rtsp conn rq action = 
  let cseq = Rtsp.msgSequence rq
      connId = connectionId conn 
  in do
    ctx <- getAuthContext rtsp connId 
    withAuthenticationResultDo rtsp conn ctx rq $ \ar -> 
      case ar of
        Right uid -> action (Just uid)
        Left MissingAuthorisation -> action Nothing
        Left err -> do 
                       return $ handleAuthFailure cseq ctx err

withAuthenticationResultDo ::       
  RtspService  ->                           -- ^ The current state of the RTSP server
  RtspConnection ->                         -- ^ The connection that we're trying to authenticate
  AuthContext ->                            -- ^ The current connection's authentication context
  Rtsp.MessageHeader ->                     -- ^ The RTSP request to authenticate
  (AuthResult UserId -> IO Rtsp.Message) -> -- ^ The action to execute on the (possibly) authenticated request
  IO Rtsp.Message                           -- ^ Returns an RTSP response to send to the client
withAuthenticationResultDo rtsp conn ctx rq action = 
  let realm = svcRealm rtsp
      connId = connectionId conn 
  in do
    now <- getPOSIXTime
    ctx <- getAuthContext rtsp connId      
    userInfo <- runErrorT $ authenticate rtsp rq ctx
    debugLog $ "Authentication returned: " ++ (show userInfo)
    action userInfo

-- | Authentcates a user - i.e. checks that the user exists and that their
--   credentials match.
--
authenticate :: 
  RtspService ->        -- ^ The RTSP service to authenticate against
  Rtsp.MessageHeader -> -- ^ The RTSP request that needs to be authenticated against
  AuthContext ->        -- ^ The current authentication context for the connection
  AuthResultIO UserId
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
  Rtsp.SequenceNumber -> -- ^ The sequence number of the RTSP request that failed authentication
  AuthContext ->         -- ^ The authoriseation context of the failed authentication request
  AuthFailure ->         -- ^ The reason that the authentication failed
  Rtsp.Message
handleAuthFailure cseq ctx reason =
  let authHeaders = genAuthHeaders ctx reason
      hdrs = Headers.setValues Rtsp.hdrAuthenticate authHeaders Headers.empty
  in (Rtsp.Response cseq Rtsp.AuthorizationRequired hdrs, Nothing)

-- | Looks up an authentication context for a given connection and returns it
--   to the caller. If no authentication context is associated with the 
--   connection then a new one is created, registered and then returned.
--
--   If the auth context is stale it wil be refreshed prior to being returned
--
getAuthContext :: RtspService -> ConnectionId -> IO AuthContext
getAuthContext rtsp@(Svc _ _ stateVar _ _) connId = 
    let realm = svcRealm rtsp
    in do now <- getPOSIXTime 
          atomically $ do state <- readTVar stateVar
                          let contexts = svcAuthContexts state 
                          case Map.lookup connId contexts of
                            Just ctx -> refreshAuthContext connId now state ctx
                            Nothing -> let ctx = newAuthContext now realm 30
                                           contexts' = Map.insert connId ctx contexts
                                           state' = contexts' `seq` state {svcAuthContexts = contexts'}
                                        in do writeTVar stateVar $! state'
                                              return ctx
  where
    -- | Refreshes the authentication context if its stale
    refreshAuthContext :: ConnectionId -> POSIXTime -> ServiceState -> AuthContext -> STM AuthContext  
    refreshAuthContext connId now state ctx = do
      if not $ contextIsStale now ctx 
        then return ctx
        else let ctx' = Authentication.refreshAuthContext now ctx  
                 ctxs = svcAuthContexts state
                 ctxs' = Map.alter (\_ -> Just ctx') connId ctxs
             in do writeTVar stateVar $! state {svcAuthContexts = ctxs'} 
                   return ctx'
                    
badRequest :: Rtsp.Message
badRequest = emptyResponse Rtsp.BadRequest 0

ok ::  Rtsp.SequenceNumber -> Rtsp.Message
ok = emptyResponse Rtsp.OK

notImplemented :: Rtsp.SequenceNumber -> Rtsp.Message
notImplemented = emptyResponse Rtsp.NotImplemented

-- | Generates an empty (i.e. has no body) response with a given status value 
emptyResponse :: Rtsp.Status -> Rtsp.SequenceNumber -> Rtsp.Message
emptyResponse status cseq = (Rtsp.Response cseq status Headers.empty, Nothing)
    
getMessageSession :: RtspService -> Rtsp.MessageHeader -> IO RtspSession
getMessageSession rtsp msg = let hdr = maybe "" id $ Rtsp.msgGetHeaderValue msg Rtsp.hdrSession
                                 sid = maybe (-1) id $ parseSessionId hdr
                             in getOrNewSession rtsp sid
                             
updateSession :: RtspService -> RtspSession -> IO ()
updateSession (Svc _ _ stateVar _ _)  session = 
  let sid = sessionId session
  in atomically $ do state <- readTVar stateVar
                     let sessions = svcSessions state
                     case Map.lookup sid sessions of
                       Just snVar -> writeTVar snVar session
                       Nothing -> return ()

parseSessionId :: String -> Maybe SessionId
parseSessionId s = case reads s of
                      [] -> Nothing
                      [(sid, _)] -> Just sid
    
-- | Fetches or creates a new RTSP session. When a new session is created has
--   the side-effect of registering the session the the service state
--
getOrNewSession :: RtspService -> SessionId -> IO RtspSession
getOrNewSession (Svc _ _ stateVar _ _) sid = do
    now <- getPOSIXTime 
    atomically $ do state <- readTVar stateVar
                    let sessions = svcSessions state
                    case Map.lookup sid sessions of
                      Just sVar -> readTVar sVar
                      Nothing -> newSession now state $ svcRng state
  where 
    -- | Recursively attempts to create a new session with a unique id
    newSession :: POSIXTime -> ServiceState -> PureMT -> STM RtspSession
    newSession now state rng = 
      let sessions = svcSessions state
          (n, rng') = randomWord64 rng
          sid = fromIntegral n
          s = Session sid now []
      in if sid `Map.member` sessions 
           then newSession now state rng'
           else do snVar <- newTVar $! s
                   let sessions' = Map.insert sid snVar sessions
                   writeTVar stateVar $! state {svcRng = rng', svcSessions = sessions'}
                   return s

serviceRealm :: RtspService -> Realm
serviceRealm (Svc r _ _ _ _) = r

serviceVersion :: RtspService -> ServerVersion
serviceVersion (Svc _ v _ _ _) = v

serviceDatabase :: RtspService -> Db
serviceDatabase (Svc _ _ _ db _) = db

-- ----------------------------------------------------------------------------
-- RTSP Connection Implementation
-- ----------------------------------------------------------------------------

type ChannelId = Int

data ConnReply = NoReply
               | OK
               | Error
               | Terminate
               | Transport RtpTransport
               deriving (Show)

type ReplyVar = TMVar ConnReply
  
data ItemToSend = MsgItem Rtsp.Message
                | Packet Rtsp.Packet
                | QuitWriter

data ConnMsg = Hello ReplyVar
             | InboundRequest Rtsp.Message
             | OutboundResponse Rtsp.Message
             | InboundPacket Rtsp.Packet
             | OutboundPacket ChannelId B.ByteString
             | GetItemToSend (TMVar ItemToSend)
             | BadRequest
             | CloseConnection
             | WriterExited
             | ReaderExited
             | CreateInterleavedTransport ChannelId ChannelId
             | DestroyInterleavedChannels [ChannelId]
             | SetChannelHandlers [(ChannelId, PacketHandler)]
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
  pendingMessages :: !(Map.Map Int Rtsp.MessageHeader),
  connChannels    :: !(Map.Map ChannelId (Maybe PacketHandler))   
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
newConnection connectionId rtsp s maxData = do
    svc <- newService (newState) (handleCall) (closeState)
    return $ MkConn connectionId svc
  where
    newState :: RtspSvc -> IO ConnInfo
    newState svc = 
      let realm = serviceRealm rtsp 
      in do debugLog "Entering new connection thread"
            readerTid <- forkIO $ reader s maxData svc
            writerTid <- forkIO $ writer s svc
            return $! State { 
              connId = connectionId,
              connHandle = (MkConn connectionId svc),
              connSocket = s,
              connState = ConnAlive,
              connRtspService = rtsp,
              readerThread = readerTid,
              writerThread = writerTid,
              writerReplyVar = Nothing,
              sendQueue = [],
              pendingMessages = Map.empty,
              connChannels = Map.empty }
      
    closeState :: ConnInfo -> IO ()
    closeState state = do 
      debugLog "Destroying connection"
      return ()
      
sendResponse :: RtspConnection -> Rtsp.Message -> IO ()
sendResponse (MkConn _ svc) msg = post svc $! OutboundResponse msg

-- | Creates a new RTSP transport that will interleave packets into the RTSP
--   control stream 
newRtspTransport :: RtspConnection -> TransportSpec -> RtspResultIO RtpTransport
newRtspTransport (MkConn _ svc) spec = 
  let params       = transportParams spec
      channels     = maybe [] (\(Interleaved cs) -> cs) $ find isInterleaved params
      (rtp:rtcp:_) = map fromIntegral channels 
  in do 
    if channels == [] || ((length channels) < 2)
      then throwError Rtsp.UnsupportedTransport
      else do rval <- liftIO $ call svc (CreateInterleavedTransport rtp rtcp)
              case rval of 
                Transport t -> return t
                _ -> throwError Rtsp.UnsupportedTransport

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
                                  
      OutboundPacket channel payload -> do state' <- sendOutboundPacket channel payload state
                                           return (NoReply, state')
      
      CreateInterleavedTransport a b -> return $ createInterleavedTransport conn (a, b) state
      
      DestroyInterleavedChannels cs -> let state' = destroyInterleavedChannels cs state
                                       in return (NoReply, state')
      
      SetChannelHandlers handlers -> let cs = connChannels state
                                         cs' = foldl' (\m (k,v) -> Map.insert k (Just v) m) cs handlers 
                                         state' = state {connChannels = cs'}
                                     in return (NoReply, state')

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
     
sendOutboundMessage :: Rtsp.Message -> ConnInfo -> IO ConnInfo
sendOutboundMessage (msg, body) state = 
  let version = (serviceVersion . connRtspService) state
      msg' = Rtsp.msgSetHeaders msg [("Server", version)]
  in putItemToSend (MsgItem (msg', body)) state
  
sendOutboundPacket :: ChannelId -> B.ByteString -> ConnInfo -> IO ConnInfo
sendOutboundPacket channel payload state = putItemToSend (Packet (Rtsp.Packet channel payload)) state
  
processBadRequest :: ConnInfo -> IO ConnInfo
processBadRequest state = 
  let (msg, body) = badRequest
  in do state' <- putItemToSend (MsgItem (msg, body)) (clearQueue state)
        disconnect state'

-- | Creates an interleaved transport 
createInterleavedTransport :: RtspConnection -> (ChannelId, ChannelId) -> ConnInfo -> (ConnReply, ConnInfo)
createInterleavedTransport conn@(MkConn _ svc) proposed@(rtp, rtcp) state = 
  let channels  = connChannels state
      register  = \(rtpH, rtcpH) -> registerTransportCallbacks conn [(rtp, rtpH), (rtcp, rtcpH)]
      txRtp     = \p -> sendPacket conn rtp p
      txRtcp    = \p -> sendPacket conn rtcp p
      spec      = TransportSpec RTP AVP TCP [Interleaved [fromIntegral rtp, fromIntegral rtcp]]
      destroy   = post svc $ DestroyInterleavedChannels [rtp, rtcp]
      channels' = Map.insert rtp Nothing $ Map.insert rtcp Nothing channels
      transport = RtpTransport { transportReg     = register,
                                 transportTxRtp   = PacketHandler txRtp,
                                 transportTxRtcp  = PacketHandler txRtcp,
                                 transportDestroy = destroy,
                                 transportGetSpec = spec,
                                 transportDisplay = "Interleaved Transport: " ++ (show rtp) ++ ", " ++ (show rtcp) }
  in if rtp `Map.member` channels || rtcp `Map.member` channels
    then (Error, state)
    else (Transport transport, state {connChannels = channels'})
    
-- | Removes the specified channels from the connections map of channel ids
--
destroyInterleavedChannels :: [ChannelId] -> ConnInfo -> ConnInfo
destroyInterleavedChannels cs state = 
  let channels' = Map.filterWithKey (\x _ -> x `notElem` cs) $ connChannels state
  in channels' `seq` state {connChannels = channels'}
  
-- | Binds the interleaved channels to the handlers that will receive the
--   packets delivered on them.
--
registerTransportCallbacks :: RtspConnection -> [(ChannelId, PacketHandler)] -> IO ()
registerTransportCallbacks (MkConn _ svc) handlers = do 
  call svc $ SetChannelHandlers handlers
  return ()
  
sendPacket :: RtspConnection -> Int -> B.ByteString -> IO ()
sendPacket (MkConn _ svc) channel payload = post svc $ OutboundPacket channel payload
    
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
internalServerError :: Int -> Rtsp.MessageHeader
internalServerError sq = Rtsp.Response sq Rtsp.InternalServerError Headers.empty
 
-- | Puts an item in the send queue. Note that if the writer thread is waiting
--   on something to send, this function will wake it up and pass the supplied 
--   item directly to it, bypassing the queue.
-- 
--   This should only ever be called on the connection's message-procesisng 
--   thread.
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
    format (MsgItem (msg, body)) = Rtsp.formatMessage msg body
    format (Packet p) = Rtsp.formatPacket p
      
-- | The state block for the reader thread
data ReaderState = RS (Maybe Rtsp.MessageHeader) Int
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

debugL = liftIO . debugLog

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