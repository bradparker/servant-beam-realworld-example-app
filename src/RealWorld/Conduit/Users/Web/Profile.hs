module RealWorld.Conduit.Users.Web.Profile
  ( Profile(..)
  , fromUser
  ) where

import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import Control.Applicative ((<*>))
import Data.Aeson (ToJSON, FromJSON)
import Data.Swagger (ToSchema)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
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
