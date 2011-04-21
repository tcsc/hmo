{-# LANGUAGE DeriveDataTypeable  #-}

module RtspConnection(
  RtspConnection,
  new,
  unitTests
) where 
  
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception(Exception, catch, finally, bracket, try)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as Utf8
import Data.Maybe
import Data.Typeable
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Test.HUnit(Test(..), assertBool, assertEqual)

import qualified Rtsp as Rtsp

data ConnReply = OK
  deriving (Show)

type ReplyVar = TMVar ConnReply
  
data ItemToSend = Message Rtsp.Message (Maybe B.ByteString)
                | Packet Rtsp.Packet

data ConnMsg = Hello ReplyVar
             | InboundMsg Rtsp.Message (Maybe B.ByteString)
             | InboundPacket Rtsp.Packet
             | GetItemToSend (TMVar ItemToSend)
             | BadRequest
             | Exit

type MsgQ = TChan ConnMsg

data RtspConnection = MkConn MsgQ

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
  msgQ <- newTChanIO
  forkIO $ runConnection s maxData msgQ
  return (MkConn msgQ)
  
-- | Defines a state block for the connection.
data ConnState = State {
  readerThread :: !ThreadId,
  writerThread :: !ThreadId
}

-- | The main connection thread. Spawns a reader thread and then starts
--   handling all of the messages sent to the connection.
runConnection :: Socket -> Int -> MsgQ -> IO ()
runConnection s maxData msgQ = do
    readerTid <- forkIO $ reader s maxData msgQ
    writerTid <- forkIO $ writer s msgQ
    loop s msgQ (State readerTid writerTid) `finally` sClose s
  where 
    loop :: Socket -> MsgQ -> ConnState -> IO ()
    loop s q state = do
      msg <- atomically $ readTChan q
      case msg of 
        Hello rpy -> do 
          atomically $ putTMVar rpy OK
          loop s q state
                        
        InboundMsg msg body -> do
          state' <- processMessage msg body state
          loop s q state'
          
        InboundPacket p -> do 
          loop s q state
          
        BadRequest -> do 
          -- send bad request response and bail
          return ()

processMessage :: Rtsp.Message -> Maybe B.ByteString -> ConnState -> IO ConnState
processMessage msg body state = do
  return state
                        
-- | Posts a message to the supplied message queue
postMessage :: ConnMsg -> MsgQ -> IO ()
postMessage msg q = atomically $ writeTChan q msg
  
-- | 
writer :: Socket -> MsgQ -> IO ()
writer s msgQ = do
    receiver <- newEmptyTMVarIO
    writeLoop s msgQ receiver `catch`
      \(ExitThread rpy) -> atomically $ putTMVar rpy OK
  where
    writeLoop :: Socket -> MsgQ -> (TMVar ItemToSend) -> IO ()
    writeLoop s msgQ rx = do
      atomically $ writeTChan msgQ (GetItemToSend rx)
      item <- atomically $ readTMVar rx
      let bytes = format item
      send s bytes
      writeLoop s msgQ rx
      
    format :: ItemToSend -> B.ByteString
    format (Message msg body) = Rtsp.formatMessage msg body
    format (Packet p) = Rtsp.formatPacket p
      
-- | The state block for the reader thread
data ReaderState = RS (Maybe Rtsp.Message) Int

      
-- | The main reader loop. Reads data from the network, parses it and sends the
--   parsed messages on to the connection manager thread
reader :: Socket -> Int -> MsgQ -> IO ()
reader s maxData q = do 
    let state = RS Nothing 0
    readLoop s maxData (B.empty) q state `catch` 
      \(ExitThread rpy) -> atomically $ putTMVar rpy OK
  where 
    readLoop :: Socket -> Int -> B.ByteString -> MsgQ -> ReaderState -> IO ()
    readLoop s maxData pending q state = do
      rdBuf <- recv s 512
      let buffer = B.append pending $ rdBuf
      (remainder, state') <- process q buffer state
      readLoop s maxData remainder q state'
      
process :: MsgQ -> B.ByteString -> ReaderState -> IO (B.ByteString, ReaderState)
process msgQ bytes state@(RS pending cLen)
  | B.length bytes == 0 = return (bytes, state)
  
  | isJust pending = do
      let msg = fromJust pending
      let contentLength = Rtsp.msgContentLength msg
      case (B.length bytes) >= contentLength of 
        False -> return (bytes, state)
        True -> do
          let (body, remainder) = B.splitAt contentLength bytes
          postMessage (InboundMsg msg (Just body)) msgQ
          let state' = RS Nothing 0
          process msgQ remainder state'
          
  | (isNothing pending) && (B.head bytes == 0x24) = do
       case Rtsp.embeddedPacket bytes of 
         Just (packet, remainder) -> do 
           postMessage (InboundPacket packet) msgQ
           process msgQ remainder state
         Nothing -> return (bytes, state)
        
  | isNothing pending = do
      case  Rtsp.extractMessageBytes bytes of 
        Just (msg, remainder) -> do 
          case (Rtsp.parseMessage msg) of 
            Nothing -> do
              postMessage BadRequest msgQ
              return (remainder, state)

            Just msg -> do 
              let cLen' = Rtsp.msgContentLength msg
              if cLen' > 0 
                then process msgQ remainder (RS (Just msg) cLen)
                else do postMessage (InboundMsg msg Nothing) msgQ
                        process msgQ remainder state
                        
        Nothing -> return (bytes, state)

-- ----------------------------------------------------------------------------
-- Unit tests
-- ----------------------------------------------------------------------------

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
  
unitTests = TestList [testProcessing]