{-# LANGUAGE DeriveDataTypeable  #-}

module ScriptExecutor(
  ScriptExecutor,
  ScriptResult,
  ScriptResultIO,
  ScriptError(..),
  new,
  translateIO,
  queryUser,
  queryMountPoint,
  queryUserRights,
  ScriptExecutor.unitTests
) where

import Control.Exception
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.STM
import Data.Typeable
import qualified Scripting.Lua as Lua  
import Test.HUnit

import qualified Logger as Log
import LuaUtils
import ScriptTypes
import Flags
import Service

data ScriptError = SyntaxError String
                 | RuntimeError String
                 | ScriptNotFound String
                 | BadResponse
                 | UndefinedError String
                 deriving (Show, Eq, Typeable)

instance Error ScriptError where
  noMsg    = UndefinedError "A script manager error"
  strMsg s = UndefinedError s

instance Exception ScriptError 
  

type ScriptResult a = Either ScriptError a 
type ScriptResultIO a = ErrorT ScriptError IO a 

data ScriptAction = GetUserInfo String
                  | GetMountPoint String
                  | GetUserRights Int Int

type Rpy = Either ScriptError LuaValue

data ScriptExecutor = SM (Service ScriptAction Rpy Lua.LuaState)

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
  e <- translateIO $ (loadFile lua fileName) `catchError` \e -> do 
                        liftIO $ do infoLog "Destroying Lua State"
                                    Lua.close lua
                        throwError e

  liftIO $ do infoLog $ "Starting script executor"
              svc <- newService (\_ -> return lua) handleCall (Lua.close)
              return (SM svc)

-- | Looks up the user information and returns it to the caller 
--
queryUser :: ScriptExecutor -> String -> ScriptResultIO (Maybe UserInfo)
queryUser se userName = callAndParseResult se (GetUserInfo userName) parseUserInfo
  
-- | Looks up information about a mount point and returns it to the caller
--
queryMountPoint :: ScriptExecutor -> String -> ScriptResultIO (Maybe MountPoint)
queryMountPoint se path = callAndParseResult se (GetMountPoint path) parseMountPoint

-- | Looks up a user's rights for a given mount point 
--
queryUserRights :: ScriptExecutor -> UserId -> Int -> ScriptResultIO UserRights
queryUserRights se (UserId uid _) mountPointId = do
  rights <- callAndParseResult se (GetUserRights uid mountPointId) parseRights
  return $ maybe [] id rights  

-- | Calls the script executor service and translates the response into whatever
--   type the caller requested
callAndParseResult :: ScriptExecutor -> ScriptAction -> Parser a -> ScriptResultIO (Maybe a)
callAndParseResult (SM svc) action parseResult = do
  reply <- liftIO $ call svc action
  case reply of 
    Left e -> throwError e
    Right value -> do 
      case value of  
        LNil -> return Nothing  
        _ -> case parseResult value of 
               Just r -> return $ Just r
               Nothing -> throwError BadResponse 

-- | Handles the callback from the underlying service, executes the appropriate
--   script and returns the semi-parsed lua response to the caller for further
--   parsing.
handleCall :: ScriptAction -> Lua.LuaState -> IO (Rpy, Lua.LuaState)
handleCall (GetUserInfo userName) lua   = run lua "getUserInfo" [LString userName]   
handleCall (GetMountPoint path) lua     = run lua "getMountPoint" [LString path]
handleCall (GetUserRights uId mpId) lua = run lua "getUserRights" [LNum (fromIntegral mpId), LNum (fromIntegral uId)]

-- | Executes the stated script, collects the result from the Lua runtime 
--   (assuming a single value) and wraps it in a LuaValue type and returns it
--   to the caller
run :: Lua.LuaState -> String -> [LuaValue] -> IO (Rpy, Lua.LuaState)
run lua scriptName args = do 
    rval <- runErrorT $ runAndCollectResult lua scriptName args
    return (rval, lua)
  where     
    runAndCollectResult :: Lua.LuaState -> String -> [LuaValue] -> ScriptResultIO LuaValue
    runAndCollectResult lua script args = do
      results <- translateIO $ runScript lua script args 1
      return $ head results 

-- | 
runScript :: Lua.LuaState -> String -> [LuaValue] -> Int -> LuaResultIO [LuaValue]
runScript lua script args results =
  bracketGlobal lua "scripts" (do
    loadField lua (-1) script
    liftIO $ foldM_ (\_ arg -> pushValue lua arg) () args
    exec lua (Lua.call lua (length args) results)
    liftIO $ collectResults lua results)

-- | Parses a lua table as a 
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
    LuaUtils.SyntaxError s  -> ScriptExecutor.SyntaxError s
    LuaUtils.RuntimeError s -> ScriptExecutor.RuntimeError s
    NotFound s              -> ScriptNotFound s
    UntypedError s          -> UndefinedError s

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
    
debugLog :: String -> IO ()
debugLog = Log.debug "scrpt"

infoLog :: String -> IO ()
infoLog = Log.info "scrpt"

errorLog :: String -> IO ()
errorLog = Log.err "scrpt"

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList []
