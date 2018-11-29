module RealWorld.Conduit.Comments.Web.Comment
  ( Comment(..)
  , fromPersisted
  ) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import qualified RealWorld.Conduit.Comments.Database.Comment as Persisted
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Profile (Profile, fromUser)

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

fromPersisted :: User -> Persisted.Comment -> Comment
fromPersisted user =
  Comment
    <$> Persisted.id
    <*> Persisted.createdAt
    <*> Persisted.updatedAt
    <*> Persisted.body
    <*> const (fromUser user)
