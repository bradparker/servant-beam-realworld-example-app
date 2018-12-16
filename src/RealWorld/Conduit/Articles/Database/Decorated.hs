module RealWorld.Conduit.Articles.Database.Decorated
  ( Decorated(..)
  ) where

import RealWorld.Conduit.Articles.Database.Article (Article)
import qualified RealWorld.Conduit.Users.Database.Decorated as User

data Decorated = Decorated
  { article :: Article
  , author :: User.Decorated
  , tagList :: Set Text
  , favoriteCount :: Int
  , favorited :: Bool
  } deriving (Show, Eq, Ord)
