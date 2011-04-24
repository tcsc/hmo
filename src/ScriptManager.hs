module ScriptManager(
  ScriptManager,
  LuaU.ScriptResult,
  new,
  unitTests
) where

import Control.Exception
import Control.Monad.Error
import qualified Scripting.Lua as Lua  
import System.Log.Logger
import Test.HUnit

import qualified LuaUtils as LuaU

data ScriptManager = M

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------
new :: String -> LuaU.ScriptResult ScriptManager
new fileName = do 
  lua <- liftIO $ do l <- Lua.newstate
                     Lua.openlibs l
                     return l

  liftIO $ infoLog $ "Compiling & executing script \"" ++ fileName ++ "\""
  LuaU.loadFile lua fileName
  liftIO $ infoLog $ "Starting script executor"
  return M
    
debugLog :: String -> IO ()
debugLog = debugM "script"

infoLog :: String -> IO ()
infoLog = infoM "script"

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------

unitTests = TestList []