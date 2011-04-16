module RtspConnection(
  RtspConnection,
  new
) where 
  
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception(Exception, catch, finally, bracket, try)
import Data.Typeable
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

data ConnReply = OK
  deriving (Show)

type ReplyVar = TMVar ConnReply
  
data ConnMsg = Hello ReplyVar

type MsgQ = TChan ConnMsg

data RtspConnection = MkConn MsgQ

instance Show (TMVar a) where
  show _ = "tmvar"

-- | An exception thrown into the reader thread to signal a shutdown request
data ExitReader = ExitReader ReplyVar 
  deriving (Show, Typeable)
instance Exception ExitReader

-- | Spawns a new connection manager thread and returns a RtspConnection that 
--   can used to address the connection
new :: Socket -> Int -> IO RtspConnection
new s maxData = do
  msgQ <- newTChanIO
  forkIO $ runConnection s maxData msgQ
  return (MkConn msgQ)
  
data ConnState = State {
  readerThread :: !ThreadId
}

-- | The main connection thread. Spawns a reader thread and then starts handling all
--   of the messages sent to the connection 
runConnection :: Socket -> Int -> MsgQ -> IO ()
runConnection s maxData msgQ = do
    tid <- forkIO $ reader s maxData msgQ
    loop s msgQ (State tid) `finally` sClose s
  where 
    loop :: Socket -> MsgQ -> ConnState -> IO ()
    loop s q state = do
      msg <- atomically $ readTChan q
      case msg of 
        Hello rpy -> do atomically $ putTMVar rpy OK
                        loop s q state

data ReaderState = RdState 

reader :: Socket -> Int -> MsgQ -> IO ()
reader s maxData q = do 
    let state = RdState 
    readLoop s maxData q state `catch` \(ExitReader rpy) -> atomically $ putTMVar rpy OK
  where 
    readLoop :: Socket -> Int -> MsgQ -> ReaderState -> IO ()
    readLoop s maxData q state = do
      recv s 1024
      readLoop s maxData q state