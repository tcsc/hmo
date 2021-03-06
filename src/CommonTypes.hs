module CommonTypes (
  UserId (..),
  UserInfo (..),
  MountPoint (..),
  UserRight (..),
  UserRights,
  fromUserInfo
) where
    
import Data.Bits

data UserId = UserId Int String
            deriving (Show)

data UserInfo = User { 
  userId :: !Int,
  userLogin :: !String,
  userPassword :: !String 
} deriving(Show)

data MountPoint = MountPoint {
  mountPointId :: !Int,
  mountPointName :: !String,
  mountPointDescription :: !String,
  mountPointEnabled :: !Bool
}

data UserRight = View
               | Broadcast
               deriving (Eq, Show)

instance Enum UserRight where 
  toEnum 1 = View
  toEnum 2 = Broadcast
  fromEnum View      = 1
  fromEnum Broadcast = 2
  succ = toEnum . (\n -> shiftL n 1) . fromEnum
  pred = toEnum . (\n -> shiftR n 1) . fromEnum
    
instance Bounded UserRight where
  minBound = View
  maxBound = Broadcast

type UserRights = [UserRight]

fromUserInfo :: UserInfo -> UserId
fromUserInfo (User userId userLogin _) = UserId userId userLogin 

-- | Pipeline operator borrowed from f#
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

