module Config (
  Config(..),
  loadConfig
) where

import Control.Exception
import Control.Monad.Error
import Data.Either
import Data.Word
import qualified Scripting.Lua as Lua  
import System.Log.Logger

import LuaUtils

data Config = Config {
  ports :: ![Word16]
} deriving (Show)

loadConfig :: String -> IO (Either String Config)
loadConfig filename = bracket (Lua.newstate) 
                              (Lua.close) 
                              (getConfig filename)
  where
    getConfig :: String -> Lua.LuaState -> IO (Either String Config)
    getConfig fileName lua = do 
      infoLog ("Loading config script: " ++ fileName)
      Lua.openlibs lua
      execLua lua (\l -> Lua.loadfile l filename) execConfig
        
    execConfig :: Lua.LuaState -> IO (Either String Config)
    execConfig lua = do
      execLua lua (\l -> do infoLog "Executing config script"
                            Lua.call l 0 0) 
                  (\l -> do { ports <- getPorts l; return $ Right (Config ports); } )
                
    execLua :: Lua.LuaState -> (Lua.LuaState -> IO Int) -> 
                               (Lua.LuaState -> IO (Either String a)) -> 
                               IO (Either String a)
    execLua lua f fnext = do 
      rval <- f lua
      case rval of 
        0 -> fnext lua
        _ -> do err <- Lua.peek lua (-1)
                let errText = case err of
                                Just s -> s
                                Nothing -> "Unexpected error"
                return (Left errText)
  
    getPorts :: Lua.LuaState -> IO [Word16]
    getPorts l = bracket_ (Lua.getglobal l "ports")
                          (Lua.pop l 1)
                          (do t <- Lua.istable l (-1)
                              case t of 
                                True -> mapTable l (\(x::Lua.LuaInteger) -> fromIntegral x)
                                False -> return [])

debugLog :: String -> IO ()
debugLog = debugM "config"

infoLog :: String -> IO ()
infoLog = infoM "config"