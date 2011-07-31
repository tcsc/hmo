module SessionManager(
  SessionManager,
  SessionError(..),
  SessionResult,
  SessionResultIO,
  newSessionManager,
  createSession,
  SessionManager.unitTests
) where
  
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Monad.Error
import Data.List
import Data.Maybe
import Test.HUnit

import CommonTypes
import qualified FileSystem as Fs
import qualified Logger as Log
import Session
import qualified Sdp as Sdp
import SessionDescription
import ScriptExecutor

data SessionManager = SM ScriptExecutor (Fs.FileSystem FsNode)
data SessionError = Unauthorised
                  | NotImplemented
                  | NotFound
                  | AlreadyExists
                  | InternalError
                  | UnknownError String
                  deriving (Eq, Show)

data FsNode = FsSession MediaSession
            | FsStream MediaStream

instance Error SessionError where
  noMsg    = UnknownError "A session manager error"
  strMsg s = UnknownError s

type SessionResult a = Either SessionError a 
type SessionResultIO a = ErrorT SessionError IO a 

liftSTM :: MonadIO m => STM a -> m a 
liftSTM = liftIO . atomically

liftFs :: Fs.FsResult a -> SessionResultIO a 
liftFs fsr = do
  x <- liftIO $ Fs.runFs fsr
  case x of 
    Right a -> return a 
    Left e -> throwError $ translateFs e
    
translateFs :: Fs.FsError -> SessionError 
translateFs f = case f of 
                  Fs.AlreadyExists -> AlreadyExists
                  _ -> InternalError

newSessionManager :: ScriptExecutor -> IO (SessionManager)
newSessionManager s = do 
  fs <- atomically $ Fs.newFileSystem
  return (SM s fs)
  
-- | Creates a new session at the specified location
--
createSession :: 
  SessionManager -> 
  String -> 
  SessionDescription -> 
  UserId -> 
  SessionResultIO MediaSession
createSession (SM scr fs) path desc uid = do
    dbgL $ "Attempting to create session at " ++ path
    (mountPoint, rights) <- getInfo scr path uid
    if Broadcast `notElem` rights
      then do dbgL "Not authorised"
              throwError Unauthorised 
      else do 
        liftIO $ debugLog "Creating new session"
        (session, uris) <- lift $ do s <- newSession desc uid
                                     streams <- getStreams s
                                     uris <- getStreamPaths path streams
                                     return (s, uris)
        
        let nodes = foldl' (\ns (p,str) -> (p, FsStream str) : ns) 
                           [(path, FsSession session)]
                           uris 
                             
        catchError (liftFs $ Fs.register nodes fs)  
                   (\sr -> do liftIO $ do debugLog $ "Error raised during session registraion: " ++ (show sr)
                                          deleteSession session
                              throwError sr)
        return session
  where
     getStreamPaths :: String -> [MediaStream] -> IO [(String, MediaStream)]
     getStreamPaths path streams = 
       let root = if ((head . reverse) path) == '/' then path else path ++ "/"
           concatUri s = streamUri s >>= \uri -> return ((root ++ uri), s)
       in mapM concatUri streams
       
-- | 
getInfo :: ScriptExecutor -> String -> UserId -> SessionResultIO (MountPoint, UserRights)
getInfo scripts path uid = do
    info <- queryInfo
    case info of
      Just x -> return x
      Nothing -> throwError NotFound
  where 
    queryInfo = translateScriptIO $ do 
      mountPoint <- queryMountPoint scripts path
      case mountPoint of
        Just mp -> do 
          rights <- getUserRights scripts uid mp 
          return $ Just (mp, rights)
        Nothing -> return Nothing

    getUserRights :: ScriptExecutor -> UserId -> MountPoint -> ScriptResultIO UserRights
    getUserRights scripts uid mp = 
      queryUserRights scripts uid (mountPointId mp)

translateScriptIO :: ScriptResultIO a -> SessionResultIO a
translateScriptIO x = do 
  s <- liftIO $ runErrorT x
  case s of 
    Left _ -> throwError InternalError
    Right a -> return a


-- ----------------------------------------------------------------------------
-- Logging
-- ----------------------------------------------------------------------------

debugLog :: String -> IO ()
debugLog = Log.debug "sessn"
  
dbgL = liftIO . debugLog
  
infoLog :: String -> IO ()
infoLog = Log.info "sessn"

errorLog :: String -> IO ()
errorLog = Log.err "sessn"

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList []