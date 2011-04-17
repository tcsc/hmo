module RtspConnection(
  RtspConnection,
  new
) where 
  
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception(Exception, catch, finally, bracket, try)
import Control.Monad
import Control.Monad.Trans
import Data.Typeable
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B

import Data.Maybe

import qualified Rtsp as Rtsp

data ConnReply = OK
  deriving (Show)

type ReplyVar = TMVar ConnReply
  
data ConnMsg = Hello ReplyVar
             | InboundMsg Rtsp.Message (Maybe B.ByteString)
             | InboundPacket Rtsp.Packet
             | BadRequest

type MsgQ = TChan ConnMsg

data RtspConnection = MkConn MsgQ

instance Show (TMVar a) where
  show _ = "tmvar"

-- | An exception thrown into the reader thread to signal a shutdown request
data ExitReader = ExitReader ReplyVar 
  deriving (Show, Typeable)
instance Exception ExitReader

--type MaybeIO a = MaybeT IO a

-- | Spawns a new connection manager thread and returns a RtspConnection that 
--   can used to address the connection
new :: Socket -> Int -> IO RtspConnection
new s maxData = do
  msgQ <- newTChanIO
  forkIO $ runConnection s maxData msgQ
  return (MkConn msgQ)
  
-- | Defines a state block for the connection.
data ConnState = State {
  readerThread :: !ThreadId
}

-- | The main connection thread. Spawns a reader thread and then starts handling all
--   of the messages sent to the connection.
runConnection :: Socket -> Int -> MsgQ -> IO ()
runConnection s maxData msgQ = do
    tid <- forkIO $ reader s maxData msgQ
    loop s msgQ (State tid) `finally` sClose s
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
          
        InboundPacket p -> loop s q state
          
        BadRequest -> do 
          -- send bad request response and bail
          return ()

processMessage :: Rtsp.Message -> Maybe B.ByteString -> ConnState -> IO ConnState
processMessage msg body state = do
  return state
                        
-- | Posts a message to the supplied message queue
postMessage :: ConnMsg -> MsgQ -> IO ()
postMessage msg q = atomically $ writeTChan q msg
  
-- | The state block for the reader thread
data ReaderState = RS (Maybe Rtsp.Message)

-- | The main reader loop. Reads data from the network, parses it and sends the
--   parsed messages on to the connection manager thread
reader :: Socket -> Int -> MsgQ -> IO ()
reader s maxData q = do 
    let state = RS Nothing
    readLoop s maxData (B.empty) q state `catch` 
      \(ExitReader rpy) -> atomically $ putTMVar rpy OK
  where 
    readLoop :: Socket -> Int -> B.ByteString -> MsgQ -> ReaderState -> IO ()
    readLoop s maxData pending q state = do
      rdBuf <- recv s 512
      let buffer = B.append pending $ rdBuf
      (remainder, state') <- process q buffer state
      readLoop s maxData remainder q state'
      
    process :: MsgQ -> B.ByteString -> ReaderState -> IO (B.ByteString, ReaderState)
    process msgQ bytes state@(RS pending)
      | B.length bytes == 0 = return (bytes, state)
      
      | isJust pending = do
          let msg = fromJust pending
          let contentLength = Rtsp.msgContentLength msg
          case (B.length bytes) >= contentLength of 
            False -> return (bytes, state)
            True -> do
              let (body, remainder) = B.splitAt contentLength bytes
              postMessage (InboundMsg msg (Just body)) msgQ
              let state' = RS Nothing
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
                  if Rtsp.msgContentLength msg > 0 
                    then process msgQ remainder (RS (Just msg))
                    else do postMessage (InboundMsg msg Nothing) msgQ
                            process msgQ remainder state