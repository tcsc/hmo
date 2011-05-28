module SessionDatabase(
  SessionDatabase,
  Session(..),
  newSessionDatabase,
) where

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import qualified Data.Map as Map

import Session
  
type SessionDb = Map.Map String (TMVar Session)    
type SessionDatabase = TMVar SessionDb
    
newSessionDatabase :: STM SessionDatabase
newSessionDatabase = do
  let db = Map.empty
  newTMVar db

-- | Creates a new session variable and stores it in the database. Returns 
--   Nothing if a session already exists for the given key
--
newSession :: SessionDatabase -> String -> STM (Maybe (TMVar Session))
newSession dbVar path = do 
  db <- readTMVar dbVar
  case Map.lookup path db of 
    Just _ -> return Nothing
    Nothing -> do s <- newEmptyTMVar
                  swapTMVar dbVar $ Map.insert path s db
                  return (Just s)
                  