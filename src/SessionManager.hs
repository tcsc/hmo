module SessionManager(
  SessionError(..),
  SessionManager.unitTests
) where
  
import Control.Concurrent.STM
import Control.Monad.Error
import System.Log.Logger
import Test.HUnit

import SessionDescription
import SessionDatabase
import ScriptExecutor
import ScriptTypes
import WorkerPool
import WorkerTypes

data SessionError = NotFound
                  | AlreadyExists
                  | InternalError
                  | Unauthorised
                  | UnknownError String  
                  
instance Error SessionError where
  noMsg    = UnknownError "A session manager error"
  strMsg s = UnknownError s

type SessionResult a = Either SessionError a 
type SessionResultIO a = ErrorT SessionError IO a 
 
data Header = Auth String

data Msg = CreateSession String SessionDescription UserId
         | Setup

data Rpy = OK  
         | Error SessionError  
         | Session

data SessionManager = SM ScriptExecutor (WorkerPool Msg Rpy)

type Headers = [Header]

-- | Creates a new session manager that uses the supplied script executor to
--   supply any user data
--
new :: ScriptExecutor -> Int -> IO SessionManager
new executor nThreads = do 
    db <- atomically $ newSessionDatabase
    pool <- newWorkerPool nThreads setup teardown (call executor db)
    return $ SM executor pool
  where
    setup :: IO ()
    setup = return ()
    
    teardown :: () -> IO ()
    teardown _ = return ()

    call :: ScriptExecutor -> SessionDatabase -> Msg -> () -> IO (Rpy, ())
    call scripts sessionDb msg _ = do
      rpy <- handleMsg scripts sessionDb msg 
      return (rpy, ())

stopSessionManager :: SessionManager -> IO ()
stopSessionManager (SM _ pool) = do
  debugLog "Stopping session manager"
  stopWorkerPool pool 

translateScriptIO :: ScriptResultIO a -> SessionResultIO a
translateScriptIO x = do 
  s <- liftIO $ runErrorT x
  case s of 
    Left _ -> throwError InternalError
    Right a -> return a

handleMsg :: ScriptExecutor -> SessionDatabase -> Msg -> IO Rpy
handleMsg scripts db (CreateSession path desc uid) = do
  result <- runErrorT $ do
    (mountPoint, rights) <- getInfo scripts path uid
    if Broadcast `elem` rights
      then return ()
      else throwError Unauthorised 
  return $ case result of 
              Left err -> Error err
              Right _ -> OK

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

-- ----------------------------------------------------------------------------
-- Logging
-- ----------------------------------------------------------------------------

debugLog :: String -> IO ()
debugLog = debugM "session"

infoLog :: String -> IO ()
infoLog = infoM "session"

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList []