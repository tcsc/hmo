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
import RtspConnection
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
        sessionManager <- SessionManager.new scripting 4

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
      
logError :: ScriptError -> IO ()
logError err = do
  let msg = case err of
              SyntaxError s ->  "script syntax error: " ++ s
              RuntimeError s -> "script runtime error: " ++ s
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
bindRtsp :: TcpListener -> Word16 -> Int -> IO ()
bindRtsp listener port cutoff = do 
  	bind listener AF_INET iNADDR_ANY (fromIntegral port) handler
  	return ()
	where 
		handler s = do 
			RtspConnection.new s cutoff
			return ()

-- |
split :: Eq a => a -> [a] -> [[a]]
split delim as = unfoldr (split' delim) as
  where
    split' _ [] = Nothing
    split' d s = 
      let (h, t) = span (/= d) s 
      in Just (h, drop 1 t) 
      
errorLog :: String -> IO ()
errorLog = Log.err " main"
 
debugLog :: String -> IO ()
debugLog = Log.debug " main"

infoLog :: String -> IO ()
infoLog = Log.info " main"
