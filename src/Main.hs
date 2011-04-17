-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Word
import IO
import Network.Socket
import qualified Scripting.Lua as Lua
import System.Posix.Signals 
import System.Log.Logger
import System.Log.Handler.Simple

import Config
import TcpListener
import RtspConnection

data Msg = Interrupt

type MsgQ = TChan Msg 

main :: IO ()
main = do 
    q <- newTChanIO
    setLogger
    infoLog "Loading configuration"
    cfg <- loadConfig "config.lua"
    
    case cfg of 
      Left err -> infoLog ("Configuration failed: " ++ err)
      Right config -> do 
        infoLog "Installing interrupt signal handler"
        installHandler sigINT (Catch $ interrupt q) Nothing

        infoLog "Creating TCP listener"        
        listener <- newListener
        startRtsp listener config
  
        infoLog "Entering main processing loop"
        mainLoop q

        infoLog "Shutting down TCP services..."
        stopListener listener
        infoLog "Exiting"
  where 
    startRtsp :: TcpListener -> Config -> IO ()
    startRtsp listener cfg = do
      infoLog "Starting RTSP listeners"
      let rtspCfg = rtspConfig cfg 
      foldM_ (\_ port -> bindRtsp listener port (rtspCutoff rtspCfg)) 
             ()
             (rtspPorts rtspCfg)
    
    mainLoop :: MsgQ -> IO ()
    mainLoop q = do 
      msg <- atomically $ readTChan q
      case msg of 
        Interrupt -> return () 
      
-- | Handles an interrupt signal from the system and sends a signal to the main
--   thread to exit
interrupt :: MsgQ -> IO ()
interrupt q = atomically $ writeTChan q Interrupt

-- | Installs and configures the default logger
setLogger :: IO ()
setLogger = do
  vl <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [vl])
  return ()

-- |
bindRtsp :: TcpListener -> Word16 -> Int -> IO ()
bindRtsp listener port cutoff = do 
  bind listener AF_INET iNADDR_ANY (PortNum port) 
       (\s -> do { RtspConnection.new s cutoff; return (); } )
  return ()

-- |
split :: Eq a => a -> [a] -> [[a]]
split delim as = unfoldr (split' delim) as
  where
    split' _ [] = Nothing
    split' d s = 
      let (h, t) = span (/= d) s 
      in Just (h, drop 1 t) 
      
debugLog :: String -> IO ()
debugLog = debugM "main"

infoLog :: String -> IO ()
infoLog = infoM "main"