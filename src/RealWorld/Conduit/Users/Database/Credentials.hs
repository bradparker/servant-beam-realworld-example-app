module RealWorld.Conduit.Users.Database.Credentials
  ( Credentials(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Swagger (ToSchema)

data Credentials = Credentials
  { email :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON Credentials
instance ToSchema Credentials
