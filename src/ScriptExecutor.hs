module ScriptExecutor(
  ScriptExecutor,
  ScriptResult,
  ScriptResultIO,
  ScriptError(..),
  new,
  translateIO,
  queryMountPoint,
  queryUserRights,
  ScriptExecutor.unitTests
) where

import Control.Exception
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.STM
import qualified Scripting.Lua as Lua  
import System.Log.Logger
import Test.HUnit

import LuaUtils
import ScriptTypes
import Flags

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

type ScriptReplyVar a = TMVar (ScriptResult a)

data ScriptAction = GetUserInfo String (ScriptReplyVar LuaValue)
                  | GetMountPoint String (ScriptReplyVar LuaValue)
                  | GetUserRights Int Int (ScriptReplyVar LuaValue)

data ScriptExecutor = SM MsgQ
data ExecState = S [ScriptAction]

-- | A function that knows how to convert something representated as a Lua value
--   into a haskell data type. 
type Parser a = LuaValue -> Maybe a

-- ----------------------------------------------------------------------------
-- The script executor is single-threaded at the moment, and runs on a dispatch
-- queue of jobs. Be careful, as a badly-written script can bollocks the entire
-- thing.
--
-- Future options to get around this include allowing the scripts to run as
-- coroutines so they can suspend themselves when they're blocked and allow
-- others to run, or having multiple script executors sharing some common data.
--
-- Not sure what the best approach is, but as the scripts are glorified config
-- files at the moment it doesn't really matter either way.
-- ----------------------------------------------------------------------------
new :: String -> ScriptResultIO ScriptExecutor
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

queryUser :: ScriptExecutor -> String -> ScriptResultIO (Maybe UserInfo)
queryUser s userName = do
  replyVar <- liftIO newEmptyTMVarIO
  callAndParseResult s (GetUserInfo userName replyVar) replyVar parseUserInfo
  
queryMountPoint :: ScriptExecutor -> String -> ScriptResultIO (Maybe MountPoint)
queryMountPoint s path = do 
  replyVar <- liftIO newEmptyTMVarIO
  callAndParseResult s (GetMountPoint path replyVar) replyVar parseMountPoint

queryUserRights :: ScriptExecutor -> Int -> Int -> ScriptResultIO UserRights
queryUserRights s uid mountPoint = do
  replyVar <- liftIO newEmptyTMVarIO
  let action = GetUserRights uid mountPoint replyVar
  rights <- callAndParseResult s action replyVar parseRights
  return $ maybe [] id rights  

callAndParseResult :: ScriptExecutor -> ScriptAction -> ScriptReplyVar LuaValue -> Parser a -> ScriptResultIO (Maybe a)
callAndParseResult s action replyVar parseResult = do
  val <- call s action replyVar
  case val of 
    LNil -> return Nothing  
    _ ->  case parseResult val of 
            Just r -> return $ Just r
            Nothing -> throwError BadResponse 

call :: ScriptExecutor -> ScriptAction -> ScriptReplyVar LuaValue -> ScriptResultIO LuaValue
call (SM q) action replyVar = do
  v <- liftIO $ do atomically $ writeTChan q $ Execute action
                   atomically $ takeTMVar replyVar
  case v of 
    Left e -> throwError e
    Right x -> return x  

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
          
-- | Handles a request to run a script by working out which script to run,
--   invoking it, parsing the response and feeting that response back to the
--   caller
executeScript :: Lua.LuaState -> ScriptAction -> MsgQ -> ExecState -> IO ExecState 
executeScript lua script msgQ state = do
  case script of 
    GetUserInfo userName rpy ->
      runAndReply lua "getUserInfo" [LString userName] rpy

    GetMountPoint path rpy ->
      runAndReply lua "getMountPoint" [LString path] rpy
  return state

-- | Executes a script with the supplied arguments, extracts & parses the
--   response and feeds the result back to the caller.
runAndReply :: Lua.LuaState -> String -> [LuaValue] -> ScriptReplyVar LuaValue -> IO ()
runAndReply lua s args rpy = do 
    rval <- runErrorT $ runAndCollectResult lua s args
    atomically $ putTMVar rpy rval
    return ()
  where     
    runAndCollectResult :: Lua.LuaState -> String -> [LuaValue] -> ScriptResultIO LuaValue
    runAndCollectResult lua script args = do
      results <- translateIO $ runScript lua script args 1
      return $ head results 
        
runScript :: Lua.LuaState -> String -> [LuaValue] -> Int -> LuaResultIO [LuaValue]
runScript lua script args results =
  bracketGlobal lua "scripts" (do
    loadField lua (-1) script
    liftIO $ foldM_ (\_ arg -> pushValue lua arg) () args
    exec lua (Lua.call lua (length args) results)
    liftIO $ collectResults lua results)

parseUserInfo :: LuaValue -> Maybe UserInfo
parseUserInfo (LTable t) = do
  uid  <- tableField (LString "id") t    >>= number
  name <- tableField (LString "login") t >>= string
  pwd  <- tableField (LString "pwd") t   >>= maybeString
  return $ User (truncate uid) name pwd
parseUserInfo _ = Nothing
  
parseMountPoint :: LuaValue -> Maybe MountPoint
parseMountPoint (LTable t) = do 
  mpid        <- tableField (LString "id") t          >>= number
  name        <- tableField (LString "name") t        >>= string 
  description <- tableField (LString "description") t >>= string
  enabled     <- tableField (LString "enabled") t     >>= boolean
  return $ MountPoint (truncate mpid) name description enabled
parseMountPoint _ = Nothing

parseRights :: LuaValue -> Maybe UserRights
parseRights (LNum n) = Just $ enumerateFlags (truncate n)
parseRights _ = Nothing  

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
  
translateError :: LuaError -> ScriptError
translateError e =
  case e of 
    LuaUtils.RuntimeError s -> ScriptExecutor.RuntimeError s
    NotFound s -> ScriptNotFound s
    
debugLog :: String -> IO ()
debugLog = debugM "script"

infoLog :: String -> IO ()
infoLog = infoM "script"


-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList []
