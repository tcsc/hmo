-- ----------------------------------------------------------------------------
-- This script is used by the media server to get information about streeaming 
-- points and users, including authentication and authorisation.
--
-- The example here is pretty self contained, but in theory you should be able
-- to do anything you want to get the information.
--
-- I wouldn't rely on any global state changes though, as subsequent calls may
-- be made on multiple instances of the script engine (e.g. getMountPoint may 
-- be called from one lua instance, and getUserInfo from another )
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- API definitions
-- ----------------------------------------------------------------------------
rights = {
  none      = 0, 
  view      = 1,
  broadcast = 2
}

-- ----------------------------------------------------------------------------
-- Utility functions
-- ----------------------------------------------------------------------------



mapUsers = function (uids)
  local result = {}
  local idx = 0
  for i,name in pairs(uids) do
    id,u = lookupUser(name)
    if u ~= nil then
      result[idx] = u
      idx = idx + 1
    end
  end
  return result
end

lookupMountPoint = function (pointName)
  for i,v in pairs(mountPoints) do
    if v.userName == userName then
      return i, v
    end
  end
  return nil
end

-- Checks membership of a table
function contains(container, item)
  for i,v in pairs(container) do
    if v == item then 
      return true
    end
  end
  return false 
end

-- ----------------------------------------------------------------------------
-- Script data
-- ----------------------------------------------------------------------------

users = {
  -- user ID 0 is reserved for the anonymous user. All other IDs are fair game
  [0] = { userName = "anonymous", password = "" },
  [1] = { userName = "trent", password = "pwd" },
}

lookupUser = function (uid)
  for i,user in pairs (users) do
    if user.userName == uid then
      return i,user
    end
  end
  return nil
end

mountPoints = {  
  [127] = { 
    path = "trent",
    name = "Trent Clarke", 
    description = "Trent's Test Point", 
    broadcasters = mapUsers( {"trent", "test"} ),
    enabled = true,
    viewers = nil 
  },
  
  [128] = {
    path = "test",
    name = "Test mount point",
    description = "Testing",
    broadcasters = {},
    enabled = false,
    viewers = {}
  }
}

dummy = function (i)
  return i,i+2
end

-- ----------------------------------------------------------------------------
-- Script hooks - don't write to global data in any of these, as it may be 
-- shared across multiple Lua instances, and I don't think there's any locking
-- ----------------------------------------------------------------------------

scripts = {
  init = function() 
  end,
  
  getMountPoint = function(path)
    pointId, point = lookupMountPoint(path)
    if point == nil then 
      return nil 
    end
    
    return {
      id = pointId, 
      name = point.name, 
      description = point.description
    }
  end,
  
  getUserInfo = function(username) 
    -- if we're after the special "anonymous" user...
    if username == nil then
      userId = 0
      user = users[0]
    else
      userId,user = lookupUser(username)
    end
    
    if user == nil then
      return nil
    end
    
    return {id = userId, login = user.userName, pwd = user.password}
  end,
  
  -- called by the media server to find out what the user can do on a given
  -- mount point
  getUserRights = function(mountPointId, userId)
    point = mountPoints[mountPointId]
    if point == nil then 
      return rights.none
    end
    
    user = users[userId]
    if user == nil then
      return rights.none
    end
    
    userRights = rights.none
    
    if contains(point.broadcasters, user) then
      userRights = userRights + rights.broadcast
    end
    
    if point.viewers ~= nil then
      if contains (point.viewers, user) then 
        userRights = userRights + rights.view
      end
    else
      userRights = userRights + rights.view
    end
     
    return userRights
  end
} 

-- ----------------------------------------------------------------------------
-- Data self-validation
-- ----------------------------------------------------------------------------

for i,v in pairs(mountPoints) do
  if type (v) == "table" then 
    if v.path == nil then 
      error ("Must set mount point path in mount point id" .. i)
    end
  
    if v.broadcasters == nil then
      error ("Must set broadcasters table in mount point \"" .. v.path .. "\" (id " .. i .. ")")
    end
  end
end
