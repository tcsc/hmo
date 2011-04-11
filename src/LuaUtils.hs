module LuaUtils(mapTable) where
  
import qualified Scripting.Lua as Lua
  
-- | Maps the values of a Lua table into a list using the supplied function.
--   Assumes that the table of interest is on the top of the Lua Interface 
--   stack
mapTable :: Lua.StackValue v => Lua.LuaState -> (v -> b) -> IO [b]
mapTable lua f = do 
    isTable <- Lua.istable lua (-1)
    case isTable of
      False -> return []
      True -> do { Lua.pushnil lua; traverse lua f []; }
  where 
    -- Assumes that the table of interest is at the top of the stack
    traverse :: Lua.StackValue a => Lua.LuaState -> (a -> b) -> [b] -> IO [b]
    traverse l fn acc = do
      x <- Lua.next l (-2) -- leaves the key at -2 and the value at -1
      case x of 
        False -> return (reverse acc)
        True -> do mval <- Lua.peek l (-1) 
                   case mval of                         
                     Nothing -> return [] 
                     Just val -> do let acc' = (fn val) : acc
                                    Lua.pop l 1
                                    traverse l fn acc'