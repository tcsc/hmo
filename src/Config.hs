module Config (
  Config(..),
  RtspConfig (..),
  LuaResultIO,
  loadConfig,
) where

import Control.Monad.Error
import Control.Exception
import Control.Monad.Maybe
import Data.Either
import Data.List
import Data.Word
import qualified Data.Map as M
import qualified Scripting.Lua as Lua  

import LuaUtils
import qualified Logger as Log

data Config = Config {
  rtspConfig :: !RtspConfig
} deriving (Show)

data RtspConfig = RTSP {
  rtspPorts :: ![Word16],
  rtspCutoff :: !Int,
  rtspRealm :: !String,
  rtspServerString :: !String
} deriving (Show)

loadConfig :: String -> LuaResultIO Config
loadConfig filename = do 
  rVal <- liftIO $ do bracket (Lua.newstate) 
                              (Lua.close) 
                              (\lua -> do { Lua.openlibs lua; runErrorT $ getConfig lua filename })
  case rVal of
    Left s -> throwError s
    Right cfg -> return cfg
                              
getConfig :: Lua.LuaState -> String -> LuaResultIO Config
getConfig lua fileName = do 
  liftIO $ infoLog ("Loading configuration script \"" ++ fileName ++ "\"")
  loadFile lua fileName
  readConfig lua

readConfig :: Lua.LuaState -> LuaResultIO Config
readConfig lua = do 
  configT <- bracketGlobal lua "config" (liftIO $ readTable lua (-1))
  let rtspCfg = readRtspConfig $! configT
  case rtspCfg of 
    Nothing -> fail "Must supply an RTSP configuration"
    Just cfg -> return (Config cfg)
    
readRtspConfig :: LuaTable -> Maybe RtspConfig
readRtspConfig cfg = do
    t <- M.lookup (LString "rtsp") cfg >>= maybeTable  
    portT  <- M.lookup (LString "ports") t >>= maybeTable 
    cutoff <- M.lookup (LString "cutoff") t >>= number
    realm  <- maybe (Just "RTSP Media Server") (Just) $ M.lookup (LString "ports") t >>= string
    svrStr <- maybe (Just "HMO RTSP/0.0.1") (Just)    $ M.lookup (LString "serverString") t >>= string
    let ports = foldl' portFold [] (M.elems portT)
    return $ RTSP ports (truncate cutoff) realm svrStr
  where
    portFold :: [Word16] -> LuaValue -> [Word16]
    portFold ps x = case x of LNum n -> (truncate n) : ps; _ -> ps
    
readPortTable :: Lua.LuaState -> LuaResultIO [Word16]
readPortTable lua = 
   liftIO $ do t <- Lua.istable lua (-1)
               case t of
                 True -> mapTable lua ((\x -> fromIntegral x) :: Lua.LuaInteger -> Word16)
                 False -> return []
        
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------

debugLog :: String -> IO ()
debugLog = Log.debug "  cfg"

infoLog :: String -> IO ()
infoLog = Log.info "  cfg"