module RealWorld.Conduit.Articles.Article
  ( Article(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import RealWorld.Conduit.Users.Profile (Profile)

data Article = Article
  { id :: Int
  , slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: Set Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  }

deriving instance Generic Article
deriving instance ToJSON Article
deriving instance ToSchema Article
deriving instance FromJSON Article
