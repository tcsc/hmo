module ScriptManager(
  ScriptManager,
  ScriptResult,
  ScriptResultIO,
  ScriptError(..),
  new,
  translateIO,
  ScriptManager.unitTests
) where

import Control.Exception
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.STM
import qualified Scripting.Lua as Lua  
import System.Log.Logger
import Test.HUnit

import LuaUtils
import UserInfo

data ScriptError = SyntaxError String
                 | RuntimeError String
                 | ScriptNotFound String
                 | BadResponse
                 | UndefinedError String
                 deriving (Show, Eq)

instance Error ScriptError where
  noMsg    = UndefinedError "A script manager error"
  strMsg s = UndefinedError s

type ScriptResult a = Either ScriptError a 
type ScriptResultIO a = ErrorT ScriptError IO a 

data Msg = Execute ScriptAction
         | RunJobs
type MsgQ = TChan Msg 

type ScriptReplyVar a = TMVar (ScriptResult (Maybe a))

data ScriptAction = GetUserInfo String (ScriptReplyVar UserInfo)
                  | GetMountPoint String (ScriptReplyVar MountPoint)

data ScriptManager = SM MsgQ
data ExecState = S [ScriptAction]

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
new :: String -> ScriptResultIO ScriptManager
new fileName = do 
  lua <- liftIO $ do l <- Lua.newstate
                     Lua.openlibs l
                     return l

  liftIO $ infoLog $ "Compiling & executing authentication script \"" ++ fileName ++ "\""
  e <- translateIO $ loadFile lua fileName

  liftIO $ do infoLog $ "Starting script executor"
              q <- newTChanIO
              t <- forkIO $ executerThread lua q
              return (SM q)
              
queryUser :: ScriptManager -> String -> ScriptResultIO (Maybe UserInfo)
queryUser (SM q) userName = do
  reply <- liftIO $ do replyVar <- newEmptyTMVarIO
                       atomically $ writeTChan q $ Execute (GetUserInfo userName replyVar)
                       atomically $ takeTMVar replyVar
  case reply of
    Right x -> return x
    Left e -> throwError e
    
executerThread :: Lua.LuaState -> MsgQ -> IO ()
executerThread lua msgQ = loop lua msgQ (S [])
  where 
    loop :: Lua.LuaState -> MsgQ -> ExecState -> IO ()
    loop lua msgQ state = do
      msg <- atomically $ readTChan msgQ
      case msg of 
        Execute script -> do 
          state' <- executeScript lua script msgQ state 
          loop lua msgQ state'
        
        RunJobs -> do
          state' <- runJobs lua msgQ state 
          loop lua msgQ state'
          
runScript :: Lua.LuaState -> String -> [LuaValue] -> Int -> LuaResultIO [LuaValue]
runScript lua script args results =
  bracketGlobal lua "scripts" (do
    loadField lua (-1) script
    liftIO $ foldM_ (\_ arg -> pushValue lua arg) () args
    exec lua (Lua.call lua (length args) results)
    liftIO $ collectResults lua results)

executeScript :: Lua.LuaState -> ScriptAction -> MsgQ -> ExecState -> IO ExecState 
executeScript lua script msgQ state = do
  case script of 
    GetUserInfo userName rpy -> do
      rval <- runErrorT $ do
        results <- translateIO $ runScript lua "getUserInfo" [LString userName] 1
        case head results of 
          LNil -> return Nothing
          LTable t -> case parseUserInfo t of
                        Just u -> return $ Just u
                        Nothing -> throwError BadResponse
          _ ->  throwError BadResponse
      atomically $ putTMVar rpy rval
      return state
      
--    GetMountPoint path rpy ->
--      rval <- runErrorT $ runScript lua "getMountPoint" [LString path] 1
--      atomically $ putTVar rpy rval
--      return state    

parseUserInfo :: LuaTable -> Maybe UserInfo
parseUserInfo t = do 
  uid  <- tableField (LString "id") t    >>= number
  name <- tableField (LString "login") t >>= string
  pwd  <- tableField (LString "pwd") t   >>= maybeString
  return $ User (truncate uid) name pwd  
      
runJobs :: Lua.LuaState -> MsgQ -> ExecState -> IO ExecState 
runJobs lua msgQ state = return state

translateIO :: LuaResultIO a -> ScriptResultIO a 
translateIO x = do
  s <- liftIO $ runErrorT x
  case s of 
    Left e -> throwError $ translateError e
    Right a -> return a

translate :: LuaResult a -> ScriptResult a 
translate x = 
  case x of 
    Left e -> Left $ translateError e
    Right a -> Right a
  
  --either (Left . translateError) (Right) x
      
translateError :: LuaError -> ScriptError
translateError e =
  case e of 
    LuaUtils.RuntimeError s -> ScriptManager.RuntimeError s
    LuaUtils.RuntimeError s -> ScriptManager.RuntimeError s
    NotFound s -> ScriptNotFound s
    
debugLog :: String -> IO ()
debugLog = debugM "script"

infoLog :: String -> IO ()
infoLog = infoM "script"

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList []