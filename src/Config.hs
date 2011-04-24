module Config (
  Config(..),
  RtspConfig,
  LuaU.ScriptResult,
  loadConfig,
  rtspPorts,
  rtspCutoff
) where

import Control.Monad.Error
import Control.Exception
import Control.Monad.Error
import Data.Either
import Data.Word
import qualified Scripting.Lua as Lua  
import System.Log.Logger

import qualified LuaUtils as LuaU

data Config = Config {
  rtspConfig :: !RtspConfig
} deriving (Show)

data RtspConfig = RTSP {
  rtspPorts :: ![Word16],
  rtspCutoff :: !Int  
} deriving (Show)

loadConfig :: String -> LuaU.ScriptResult Config
loadConfig filename = do 
  rVal <- liftIO $ do bracket (Lua.newstate) 
                              (Lua.close) 
                              (\lua -> runErrorT $ getConfig lua filename)
  case rVal of
    Left s -> fail s
    Right cfg -> return cfg
                              
getConfig :: Lua.LuaState -> String -> LuaU.ScriptResult Config
getConfig lua fileName = do 
  liftIO $ do infoLog ("Loading configuration script \"" ++ fileName ++ "\"")
              Lua.openlibs lua
  LuaU.loadFile lua fileName
  readConfig lua

readConfig :: Lua.LuaState -> LuaU.ScriptResult Config
readConfig lua = do 
  rtsp <- liftIO $ readRtspConfig lua
  case rtsp of 
    Nothing -> fail "Must supply an RTSP configuration"
    Just rtspCfg -> return (Config rtspCfg)

readRtspConfig :: Lua.LuaState -> IO (Maybe RtspConfig)
readRtspConfig lua = LuaU.bracketGlobal lua "rtsp" readRtsp
  where 
    readRtsp :: Lua.LuaState -> IO (Maybe RtspConfig)
    readRtsp lua = do
      t <- Lua.istable lua (-1)
      case t of
        False -> return Nothing 
        True -> do 
          ports <- LuaU.bracketField lua "ports" readPortTable
          cutoff <- LuaU.bracketField lua "cutoff" (\l -> do x <- Lua.peek l (-1)
                                                             case x of 
                                                               Just n -> return n
                                                               Nothing -> return 1024 )
          return $ Just (RTSP ports cutoff)
        
readPortTable :: Lua.LuaState -> IO [Word16]
readPortTable lua = do 
   t <- Lua.istable lua (-1)
   case t of
     True -> LuaU.mapTable lua ((\x -> fromIntegral x) :: Lua.LuaInteger -> Word16)
     False -> return []
        
debugLog :: String -> IO ()
debugLog = debugM "config"

infoLog :: String -> IO ()
infoLog = infoM "config"