module RealWorld.Conduit.Articles.Web.Articles
  ( Articles(..)
  ) where

import RealWorld.Conduit.Articles.Article (Article)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema)

data Articles = Articles
  { articles :: [Article]
  , articlesCount :: Int
  }

deriving instance Generic Articles
deriving instance ToJSON Articles
deriving instance ToSchema Articles
deriving instance FromJSON Articles
