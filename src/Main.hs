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
        infoLog "Setting signal handler"
        installHandler sigINT (Catch $ interrupt q) Nothing

        infoLog "Starting listeners"    
        listener <- newListener
        foldM_ (\_ port -> bindPort listener port) () (ports config)
  
        infoLog "Entering main processing loop"
        mainLoop q

        infoLog "Shutting down TCP services..."
        stopListener listener
        infoLog "Exiting"
  where 
    mainLoop :: MsgQ -> IO ()
    mainLoop q = do 
      msg <- atomically $ readTChan q
      case msg of 
        Interrupt -> return () 
        _ -> mainLoop q
      
interrupt :: MsgQ -> IO ()
interrupt q = atomically $ writeTChan q Interrupt
  
setLogger :: IO ()
setLogger = do
  vl <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [vl])
  return ()

bindPort :: TcpListener -> Word16 -> IO ()
bindPort listener port = do 
  bind listener AF_INET iNADDR_ANY (PortNum port) newConnection
  return ()

newConnection :: Socket -> IO ()
newConnection s = return ()

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