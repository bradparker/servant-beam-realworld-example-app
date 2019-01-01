module RealWorld.Conduit.Articles.Web.Articles
  ( Articles(..)
  , fromList
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema)
import Prelude hiding (fromList)
import RealWorld.Conduit.Articles.Article (Article)

data Articles = Articles
  { articles :: [Article]
  , articlesCount :: Int
  }

deriving instance Generic Articles
deriving instance ToJSON Articles
deriving instance ToSchema Articles
deriving instance FromJSON Articles

fromList :: [Article] -> Articles
fromList = Articles <$> id <*> length
