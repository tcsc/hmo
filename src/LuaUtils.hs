module LuaUtils(mapTable, bracketField, bracketGlobal, bracketStack) where
  
import Scripting.Lua
import Control.Exception

-- | Maps the values of a Lua table into a list using the supplied function.
--   Assumes that the table of interest is on the top of the Lua Interface 
--   stack
mapTable :: StackValue v => LuaState -> (v -> b) -> IO [b]
mapTable lua f = do 
    isTable <- istable lua (-1)
    case isTable of
      False -> return []
      True -> do { pushnil lua; traverse lua f []; }
  where 
    -- Assumes that the table of interest is at the top of the stack
    traverse :: StackValue a => LuaState -> (a -> b) -> [b] -> IO [b]
    traverse l fn acc = do
      x <- next l (-2) -- leaves the key at -2 and the value at -1
      case x of 
        False -> return (reverse acc)
        True -> do mval <- peek l (-1) 
                   case mval of                         
                     Nothing -> return [] 
                     Just val -> do let acc' = (fn val) : acc
                                    pop l 1
                                    traverse l fn acc'

bracketStack :: LuaState -> (LuaState -> IO()) -> (LuaState -> IO a) -> IO a
bracketStack lua open fn = bracket_ (open lua) (pop lua 1) (fn lua)

bracketField :: LuaState -> String -> (LuaState -> IO a) -> IO a
bracketField lua name fn = bracketStack lua (\l -> getfield l (-1) name) fn
                                         
bracketGlobal :: LuaState -> String -> (LuaState -> IO a) -> IO a
bracketGlobal lua name action = bracketStack lua (\l -> getglobal l name) action
          
