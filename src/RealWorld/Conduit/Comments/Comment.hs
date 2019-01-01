module RealWorld.Conduit.Comments.Comment
  ( Comment(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import RealWorld.Conduit.Users.Profile (Profile)

data Comment = Comment
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , body :: Text
  , author :: Profile
  }

deriving instance Generic Comment
deriving instance ToJSON Comment
deriving instance ToSchema Comment
