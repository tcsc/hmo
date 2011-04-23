module Config (
  Config(..),
  RtspConfig,
  loadConfig,
  rtspPorts,
  rtspCutoff
) where

import Control.Exception
import Control.Monad.Error
import Data.Either
import Data.Word
import qualified Scripting.Lua as Lua  
import System.Log.Logger

import LuaUtils

data Config = Config {
  rtspConfig :: !RtspConfig
} deriving (Show)

data RtspConfig = RTSP {
  rtspPorts :: ![Word16],
  rtspCutoff :: !Int  
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
                  readConfig
                  
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

readConfig :: Lua.LuaState -> IO (Either String Config)
readConfig lua  = do 
  rtsp <- readRtspConfig lua
  case rtsp of 
    Nothing -> return $ Left "No RTSP config"
    Just rtspCfg -> return $ Right (Config rtspCfg)

readRtspConfig :: Lua.LuaState -> IO (Maybe RtspConfig)
readRtspConfig lua = bracketGlobal lua "rtsp" readRtsp
  where 
    readRtsp :: Lua.LuaState -> IO (Maybe RtspConfig)
    readRtsp lua = do
      t <- Lua.istable lua (-1)
      case t of
        False -> return Nothing 
        True -> do 
          ports <- bracketField lua "ports" readPortTable
          cutoff <- bracketField lua "cutoff" (\l -> do x <- Lua.peek l (-1)
                                                        case x of 
                                                          Just n -> return n
                                                          Nothing -> return 1024 )
          return $ Just (RTSP ports cutoff)
        
readPortTable :: Lua.LuaState -> IO [Word16]
readPortTable lua = do 
   t <- Lua.istable lua (-1)
   case t of
     True -> mapTable lua ((\x -> fromIntegral x) :: Lua.LuaInteger -> Word16)
     False -> return []
        
debugLog :: String -> IO ()
debugLog = debugM "config"

infoLog :: String -> IO ()
infoLog = infoM "config"