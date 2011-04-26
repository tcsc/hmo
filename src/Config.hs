module Config (
  Config(..),
  RtspConfig,
  LuaResultIO,
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

import LuaUtils

data Config = Config {
  rtspConfig :: !RtspConfig
} deriving (Show)

data RtspConfig = RTSP {
  rtspPorts :: ![Word16],
  rtspCutoff :: !Int  
} deriving (Show)

loadConfig :: String -> LuaResultIO Config
loadConfig filename = do 
  rVal <- liftIO $ do bracket (Lua.newstate) 
                              (Lua.close) 
                              (\lua -> runErrorT $ getConfig lua filename)
  case rVal of
    Left s -> throwError s
    Right cfg -> return cfg
                              
getConfig :: Lua.LuaState -> String -> LuaResultIO Config
getConfig lua fileName = do 
  liftIO $ do infoLog ("Loading configuration script \"" ++ fileName ++ "\"")
              Lua.openlibs lua
  loadFile lua fileName
  readConfig lua

readConfig :: Lua.LuaState -> LuaResultIO Config
readConfig lua = do 
  rtsp <- liftIO $ readRtspConfig lua
  case rtsp of 
    Nothing -> fail "Must supply an RTSP configuration"
    Just rtspCfg -> return (Config rtspCfg)

readRtspConfig :: Lua.LuaState -> IO (Maybe RtspConfig)
readRtspConfig lua = do 
    rval <- runErrorT $ bracketGlobal lua "rtsp" (readRtsp lua)
    case rval of
      Right cfg -> return cfg
      Left _ -> return Nothing
  where 
    readRtsp :: Lua.LuaState -> LuaResultIO (Maybe RtspConfig)
    readRtsp lua = do
      t <- liftIO $ Lua.istable lua (-1)
      case t of
        False -> throwError (NotFound "")
        True -> do 
          ports <- bracketField lua (-1) "ports" (readPortTable lua)
          cutoff <- bracketField lua (-1) "cutoff" (do x <- liftIO $ Lua.peek lua (-1)
                                                       case x of 
                                                         Just n -> return n
                                                         Nothing -> return 1024 )
          return $ Just (RTSP ports cutoff)
        
readPortTable :: Lua.LuaState -> LuaResultIO [Word16]
readPortTable lua = 
   liftIO $ do t <- Lua.istable lua (-1)
               case t of
                 True -> mapTable lua ((\x -> fromIntegral x) :: Lua.LuaInteger -> Word16)
                 False -> return []
        
debugLog :: String -> IO ()
debugLog = debugM "config"

infoLog :: String -> IO ()
infoLog = infoM "config"