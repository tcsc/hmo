module Service where 
  
import Control.Exception(SomeException, bracket, try)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.STM
import Data.List
import Data.Maybe
import System.Timeout(timeout)
import System.Log.Logger

import WorkerTypes

data Server msg rpy = Svr (WkrQ msg rpy)  

-- | An erlang-style service framework

--newServer :: Setup state -> MessageHandler -> Teardown state -> Server msg rpy state
--newServer setup handler teardown = do  