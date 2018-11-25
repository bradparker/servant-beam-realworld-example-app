module RealWorld.Conduit.Articles.Database.Decorated
  ( Decorated(..)
  ) where

import RealWorld.Conduit.Articles.Database.Article (Article)
import RealWorld.Conduit.Users.Database.User (User)

data Decorated = Decorated
  { article :: Article
  , author :: User
  , tagList :: Set Text
  , favoriteCount :: Int
  , favorited :: Bool
  } deriving (Show, Eq, Ord)
