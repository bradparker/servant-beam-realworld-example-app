module RealWorld.Conduit.Articles.Web.Article
  ( Article(..)
  , fromDecorated
  ) where

import Control.Applicative ((<*>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Bool (Bool)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Set (Set)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Database.Decorated (Decorated(..))
import qualified RealWorld.Conduit.Articles.Database.Decorated as Decorated
import RealWorld.Conduit.Users.Web.Profile (Profile, fromUser)

data Article = Article
  { slug :: Text
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

fromDecorated :: Decorated -> Article
fromDecorated =
  Article
    <$> Persisted.slug . Decorated.article
    <*> Persisted.title . Decorated.article
    <*> Persisted.description . Decorated.article
    <*> Persisted.body . Decorated.article
    <*> Decorated.tagList
    <*> Persisted.createdAt . Decorated.article
    <*> Persisted.updatedAt . Decorated.article
    <*> Decorated.favorited
    <*> Decorated.favoriteCount
    <*> fromUser . Decorated.author
