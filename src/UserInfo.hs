module UserInfo (
  UserInfo (..),
  MountPoint (..),
  UserRight,
  UserRights
) where
  
data UserInfo = User { 
  userId :: !Int,
  userLogin :: !String,
  userPassword :: !String 
} deriving(Show)

data MountPoint = MountPoint {
  mountPointId :: !Int,
  mountPointName :: !String,
  mountPointDescription :: !String
}

data UserRight = Broadcast
               | View

type UserRights = [UserRight]
