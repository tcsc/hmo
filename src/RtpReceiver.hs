-- | Defines an RTP reciever that received RTP packets from a transport layer
--   and turns them into abstract "chunks" that can be routed by the media
--   server independently of the actual protocols used for the end points.

module RtpReceiver (
  RtpReceiver,
  newRtpReceiver,
  destroyRtpReceiver,
  getChunkSource
) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS

import qualified Logger as Log
import MediaChunk
import RtpTransport
import Service

data RxMsg = BindReceiver ChunkHandler (TMVar ())
           | Destroy

type RxSvc = Service RxMsg State
data RtpReceiver = Rx RxSvc

data State = State {
  rtpTransport :: RtpTransport,
  rtpChunkHandler :: Maybe ChunkHandler
}
        
newRtpReceiver :: RtpTransport -> IO RtpReceiver
newRtpReceiver transport = do 
   svc <- newService (newState) (handleCall) (closeState)
   return $ Rx svc
  where
    newState :: RxSvc -> IO State
    newState _ = return $ State {rtpTransport = transport, 
                                 rtpChunkHandler = Nothing} 
      
    closeState :: State -> IO ()
    closeState state = do
      transportDestroy $ rtpTransport state
      
destroyRtpReceiver :: RtpReceiver -> IO ()
destroyRtpReceiver (Rx svc) = stopService svc

getChunkSource :: RtpReceiver -> IO ChunkSource
getChunkSource (Rx svc) = 
  let register = \h -> do { call svc (BindReceiver h); return () }
  in do return $ ChunkSource {chunkSourceRegister = register} 
        
handleCall :: RxMsg -> State -> IO State
handleCall msg state = do 
  case msg of
    BindReceiver _ rpyVar -> do reply rpyVar ()
                                return state
      
-- ----------------------------------------------------------------------------
-- Debugging & Logging
-- ----------------------------------------------------------------------------
      
errorLog :: String -> IO ()
errorLog = Log.err "rtprx"

debugLog :: String -> IO ()
debugLog = Log.debug "rtprx"

--debugL = liftIO . debugLog

infoLog :: String -> IO ()
infoLog = Log.info "rtprx"