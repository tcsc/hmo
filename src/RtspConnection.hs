{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module RtspConnection(
  RtspConnection,
  RtspConnection.new,
  RtspConnection.unitTests
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
import Data.Maybe
import Data.Typeable
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Test.HUnit(Test(..), assertBool, assertEqual)

import Authentication
import qualified Rtsp as Rtsp
import qualified Headers as Headers
import ScriptExecutor
import ScriptTypes
import Service

data ConnReply = NoReply
               | OK
  deriving (Show)

type ReplyVar = TMVar ConnReply
  
data ItemToSend = Message Rtsp.Message (Maybe B.ByteString)
                | Packet Rtsp.Packet

data ConnMsg = Hello ReplyVar
             | InboundMsg Rtsp.Message (Maybe B.ByteString)
             | InboundPacket Rtsp.Packet
             | GetItemToSend (TMVar ItemToSend)
             | BadRequest
             
-- | Defines a state block for the connection.
data ConnState = State {
  readerThread :: !ThreadId,
  writerThread :: !ThreadId,
  writerReplyVar :: !(Maybe (TMVar ItemToSend)),
  sendQueue :: ![ItemToSend],
  pendingMessages :: !(Map.Map Int Rtsp.Message)
}

type RtspSvc = Service ConnMsg ConnReply ConnState
  
data RtspConnection = MkConn RtspSvc

instance Show (TMVar a) where
  show _ = "tmvar"

-- | An exception thrown into the reader thread to signal a shutdown request
data ExitThread = ExitThread ReplyVar
  deriving (Show, Typeable)
instance Exception ExitThread

-- | Spawns a new connection manager thread and returns a RtspConnection that 
--   can used to address the connection
new :: Socket -> Int -> IO RtspConnection
new s maxData = do
    svc <- newService (newState s maxData) (handleCall) (closeState)
    return $ MkConn svc
  where
    newState :: Socket -> Int -> RtspSvc -> IO ConnState
    newState sk cutoff svc = do
      readerTid <- forkIO $ reader sk cutoff svc
      writerTid <- forkIO $ writer sk svc
      return $ State readerTid writerTid Nothing [] Map.empty
      
    closeState :: ConnState -> IO ()
    closeState state = return ()
    

-- | Handle 
handleCall :: ConnMsg -> ConnState -> IO (ConnReply, ConnState)
handleCall (InboundMsg msg body) state = processMessage msg body state >>= \state' -> return (NoReply, state')
handleCall (InboundPacket pkt) state = return (NoReply, state)
handleCall (GetItemToSend rx) state = getItemToSend rx state >>= \state' -> return (NoReply, state')
handleCall _ state = return (NoReply, state)

-- | Handles the receipt of a message and begins processing it
processMessage :: Rtsp.Message -> Maybe B.ByteString -> ConnState -> IO ConnState
processMessage rq@(Rtsp.Request sq verb uri version headers) body state = do 
    (r, state') <- do case verb of 
                        -- Rtsp.Announce -> handleAnnounce rq body state
                        _ -> notImplemented state
                     `catch` \(e :: ScriptError) -> return (internalServerError sq, state)
    putItemToSend (Message r Nothing) state'
  where
    notImplemented state = return $ (Rtsp.Response sq Rtsp.NotImplemented Headers.empty, state)

-- | 
{-
handleAnnounce :: Rtsp.Request -> Maybe B.ByteString -> ConnState -> IO ConnState
handleAnnounce rq body state = do
  userInfo <- authenticate rq 
  case userInfo of
    Nothing -> -- generate auth info and response
    Just userId -> 
-}

liftMaybe = MaybeT . return

runScript :: ScriptResultIO (Maybe a) -> MaybeT IO a
runScript action = do
  rval <- (lift . runErrorT) action
  case rval of 
    Right x -> liftMaybe x
    Left e -> throw e

authenticate :: Rtsp.Message -> ScriptExecutor -> MaybeT IO UserId
authenticate (Rtsp.Request _ verb uri _ hs) scripts = do
  authInfo <- liftMaybe $ Headers.get "authorization" hs >>= parseAuthHeader
  userInfo <- runScript $ queryUser scripts (authUser authInfo)
  case checkCreds authInfo userInfo of
    True -> return $ fromUserInfo userInfo
    False -> fail ""
  
internalServerError :: Int -> Rtsp.Message
internalServerError sq = Rtsp.Response sq Rtsp.InternalServerError Headers.empty
 
-- | Puts an item in the send queue. Note that if the writer thread is waiting
--   on something to send, this function will wake it up and pass the supplied 
--   item directly to it, bypassing the queue.
-- 
--   This should only ever be called on the connection thread
putItemToSend :: ItemToSend -> ConnState -> IO ConnState
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
--   This should only ever be called on the connection thread
getItemToSend :: TMVar ItemToSend -> ConnState -> IO ConnState
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
    receiver <- newEmptyTMVarIO
    writeLoop s svc receiver `catch`
      \(ExitThread rpy) -> atomically $ putTMVar rpy OK
  where
    writeLoop :: Socket -> RtspSvc -> (TMVar ItemToSend) -> IO ()
    writeLoop s svc rx = do
      post svc (GetItemToSend rx)
      item <- atomically $ takeTMVar rx
      let bytes = format item
      send s bytes
      writeLoop s svc rx
      
    format :: ItemToSend -> B.ByteString
    format (Message msg body) = Rtsp.formatMessage msg body
    format (Packet p) = Rtsp.formatPacket p
      
-- | The state block for the reader thread
data ReaderState = RS (Maybe Rtsp.Message) Int
  deriving (Show)
      
-- | The main reader loop. Reads data from the network, parses it and sends the
--   parsed messages on to the connection manager thread
reader :: Socket -> Int -> RtspSvc -> IO ()
reader s maxData svc = do 
    let state = RS Nothing 0
    readLoop s maxData (B.empty) svc state `catch` 
      \(ExitThread rpy) -> atomically $ putTMVar rpy OK
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
              return (remainder, state)

            Just msg -> do 
              let cLen' = Rtsp.msgContentLength msg
              if cLen' > 0 
                then process svc remainder (RS (Just msg) cLen)
                else do post svc (InboundMsg msg Nothing)
                        process svc remainder state
                        
        Nothing -> return (bytes, state)

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