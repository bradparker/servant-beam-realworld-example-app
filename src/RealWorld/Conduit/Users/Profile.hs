module RealWorld.Conduit.Users.Profile
  ( Profile(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Swagger (ToSchema)

data Profile = Profile
  { id :: Int
  , username :: Text
  , bio :: Text
  , image :: Maybe Text
  , following :: Bool
  } deriving (Generic)

deriving instance ToJSON Profile
deriving instance FromJSON Profile
deriving instance ToSchema Profile
