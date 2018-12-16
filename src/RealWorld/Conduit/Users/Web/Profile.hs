module RealWorld.Conduit.Users.Web.Profile
  ( Profile(..)
  , fromUser
  , fromDecoratedUser
  ) where

import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.Decorated (Decorated)
import qualified RealWorld.Conduit.Users.Database.Decorated as Decorated
import Control.Applicative ((<*>))
import Data.Aeson (ToJSON, FromJSON)
import Data.Swagger (ToSchema)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  } deriving (Generic)

deriving instance ToJSON Profile
deriving instance FromJSON Profile
deriving instance ToSchema Profile

fromUser :: User -> Profile
fromUser =
  Profile
    <$> User.username
    <*> User.bio
    <*> User.image
    <*> const False

fromDecoratedUser :: Decorated -> Profile
fromDecoratedUser =
  Profile
    <$> (User.username . Decorated.user)
    <*> (User.bio . Decorated.user)
    <*> (User.image . Decorated.user)
    <*> Decorated.following
