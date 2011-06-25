module Logger(debug, info, err) where

import Control.Concurrent (myThreadId)
import System.Log.Logger

debug :: String -> String -> IO ()
debug cat msg = logOut debugM cat msg
  
info :: String -> String -> IO ()
info cat msg = logOut infoM cat msg 

err :: String -> String -> IO ()
err cat msg = logOut errorM cat msg

logOut :: (String -> String -> IO ()) -> String -> String -> IO ()
logOut a cat msg = myThreadId >>= \tid -> a cat $ "\t" ++ (show tid) ++ "\t" ++ msg