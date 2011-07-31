module Session(
  MediaSession,
  MediaStream,
  newSession,
  getStreams,
  deleteSession,
  streamUri
) where 

import Control.Monad
import Data.Maybe

import CommonTypes
import Service
import SessionDescription
import qualified Logger as Log

data SsnMsg = SsnGetStreams
data SsnRpy = SsnStreams [MediaStream]
            | SsnOK
data MediaSession = Session (Service SsnMsg SsnRpy SessionState)

data SessionState = SsnState {
  ssnStreams :: ![MediaStream],
  ssnDesc :: SessionDescription
}

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

newSession :: SessionDescription -> UserId -> IO MediaSession
newSession desc uid =
  do 
    streams <- mapM newStream (sessionStreams desc)
    svc <- newService (setup (catMaybes streams) desc) handleSessionCall teardown
    return $ Session svc
  where 
    setup streams desc svc = return $ SsnState streams desc
    teardown _ = return ()
    
getStreams :: MediaSession -> IO [MediaStream]
getStreams (Session svc) = do
  SsnStreams ss <- call svc SsnGetStreams
  return ss
  
handleSessionCall :: SsnMsg -> SessionState -> IO (SsnRpy, SessionState)
handleSessionCall msg state = 
  case msg of 
    SsnGetStreams -> let ss = ssnStreams state
                     in return $! (SsnStreams ss, state)
  
deleteSession :: MediaSession -> IO ()
deleteSession (Session svc) = stopService svc

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

data StrMsg = StrGetUri
            | StrSetup
            | StrPlay
            | StrPause
            | StrTeardown
data StrRpy = StrOK
            | StrUri String
data StreamState = Stream {
  ssnUri :: String
  }
data MediaStream = Str (Service StrMsg StrRpy StreamState)

newStream :: StreamDescription -> IO (Maybe MediaStream)
newStream streamDesc = 
  let majorType = streamType streamDesc
      params = streamParams streamDesc
      uri = streamControlUri streamDesc
  in case uri of
      Nothing -> do
        infoLog "Control URI missing for stream - not creating a thread for it"
        return Nothing
         
      Just controlUri -> do 
        debugLog $ "Creating new stream for " ++ 
                      (foldl (\s e -> s ++ e ++ " ") "" $ streamEncodings streamDesc)
        svc <- newService (setup controlUri) handleStreamCall teardown
        return $ Just (Str svc)
  where 
    setup uri svc = return $ Stream uri
    teardown _ = return ()
    
-- | gets the relative uri for controlling the stream  
streamUri :: MediaStream -> IO String
streamUri (Str svc) = do StrUri s <- call svc StrGetUri
                         return s 

handleStreamCall :: StrMsg -> StreamState -> IO (StrRpy, StreamState)
handleStreamCall msg state = do 
  case msg of
    StrGetUri -> return (StrUri $ ssnUri state, state)

-- ----------------------------------------------------------------------------
-- Logging
-- ----------------------------------------------------------------------------

debugLog :: String -> IO ()
debugLog = Log.debug "  ssn"

infoLog :: String -> IO ()
infoLog = Log.info "  ssn"

errorLog :: String -> IO ()
errorLog = Log.err "  ssn"