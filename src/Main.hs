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
import System.Log.Logger
import System.Log.Handler.Simple

import Config
import ScriptExecutor
import TcpListener
import RtspService
import SessionManager
import Signals
import qualified Logger as Log

data Msg = Interrupt
type MsgQ = TChan Msg 

main :: IO ()
main = withSocketsDo $ do 
    q <- newTChanIO
    setLogger
    infoLog "Loading configuration"
    
    initR <- runErrorT $ do 
      cfg <- translateIO $ loadConfig "config.lua"
      script <- ScriptExecutor.new "authentication.lua"
      return (cfg, script)
      
    case initR of 
      Left err -> do logError err
                     infoLog "exiting"
                     
      Right (config, scripting) -> do 
        infoLog "Installing interrupt signal handler"
        installInterruptHandler (interrupt q)

        infoLog "Starting Session Manager"
        sessionManager <- newSessionManager scripting

        infoLog "Creating RTSP Service"
        rtspSvc <- createRtspService scripting config

        infoLog "Creating TCP listeners for RTSP"        
        listener <- newListener
        startRtsp listener rtspSvc config
  
        infoLog "Entering main processing loop"
        mainLoop q

        infoLog "Shutting down TCP services..."
        stopListener listener
        infoLog "Exiting"
  where 
    createRtspService :: ScriptExecutor -> Config -> IO RtspService
    createRtspService scripts cfg = let rtspCfg = rtspConfig cfg
                                        realm = rtspRealm rtspCfg
                                        svr = rtspServerString rtspCfg
                                    in newRtspService realm svr scripts

    startRtsp :: TcpListener -> RtspService -> Config -> IO ()
    startRtsp listener service cfg = do
      infoLog "Starting RTSP listeners"
      let rtspCfg = rtspConfig cfg 
      let cutoff = rtspCutoff rtspCfg
      foldM_ (\_ port -> bindRtsp listener port (onRtspConnection service cutoff) ) 
             ()
             (rtspPorts rtspCfg)
    
    mainLoop :: MsgQ -> IO ()
    mainLoop q = do 
      msg <- atomically $ readTChan q
      case msg of 
        Interrupt -> return () 
        
    onRtspConnection :: RtspService -> Int -> Socket -> IO ()
    onRtspConnection rtsp cutoff s = handleConnection rtsp cutoff s >> return ()
      
      
logError :: ScriptError -> IO ()
logError err = do
  let msg = case err of
              SyntaxError s ->  "script syntax error: " ++ s
              RuntimeError s -> "script runtime error: " ++ s
              s -> show s
  errorLog msg
              
      
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
bindRtsp :: TcpListener -> Word16 -> ConnectionHandler -> IO ()
bindRtsp l port handler = bindListener l AF_INET iNADDR_ANY (fromIntegral port) handler
  				
-- ----------------------------------------------------------------------------
-- Logging
-- ----------------------------------------------------------------------------
      
errorLog :: String -> IO ()
errorLog = Log.err " main"
 
debugLog :: String -> IO ()
debugLog = Log.debug " main"

infoLog :: String -> IO ()
infoLog = Log.info " main"
