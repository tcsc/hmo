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
-- Script data
-- ----------------------------------------------------------------------------

users = {
  -- user ID 0 is reserved for the anonymous user. All other IDs are fair game
  [0] = { userName = "anonymous", password = nil },
  [1] = { userName = "trent", passsword = "pwd" },
}

mountPoints = {  
  [127] = { 
    path = "trent",
    name = "Trent Clarke", 
    description = "Trent's Test Point", 
    broadcasters = { 1 },
    enabled = true,
    viewers = nil 
  },
  
  [128] = {
    path = "test",
    name = "Test mount point",
    description = "Testing",
    broadcasters = {}, -- 
    enabled = false,
    viewers = {}
  }
}

-- ----------------------------------------------------------------------------
-- Data validation
-- ----------------------------------------------------------------------------
for i,v in pairs(mountPoints) do
  if v.path == nil then 
    error ("Must set mount point path in mount point id" .. i)
  end
  
  if v.broadcasters == nil then
    error ("Must set broadcasters table in mount point \"" .. v.path .. "\" (id " .. i .. ")")
  end
end

-- ----------------------------------------------------------------------------
-- Utility functions
-- ----------------------------------------------------------------------------

users.lookup = function (userName)
  for i,v in pairs(users) do
    if v.userName == userName then
      return i,v
    end
  end
  return nil
end

mountPoints.lookup = function (pointName)
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
-- Script hooks
-- ----------------------------------------------------------------------------

scripts = {
  init = function() 
  end,
  
  getMountPoint = function(path)
    pointId, point = mountPoints.lookup(path)
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
      userId,user = users.lookup(username)
    end
    
    if user == nil then
      return nil
    end
    
    return {id = userId, uid = user.name, pwd = user.password}
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
  end,
  
  -- Called by the media server to check if a given user is allowed to
  -- broadcast on a given mount point 
  userCanBroadcast = function(userName, pointName)
  end,
  
  -- Called by the media server to check is a logged-in user can view a
  -- stream
  userCanView = function(userName, pointName)
    point = mountPoints.pointName
    if point == nil then 
      return false
    end
    
    -- if the viewers table is nil then we assume it's a free-for all
    if point.viewers == nil then 
      return true 
    end 
    
    user = users.userName 
    if user == nil then 
      return false 
    end
    
    -- membership in the viewers table implies that the user is allowed to view
    -- the stream
    return contains(point.viewers, user) 
  end
}  