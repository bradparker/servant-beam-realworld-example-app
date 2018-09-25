module RealWorld.Conduit.Articles.Database.Decorated
  ( Decorated(..)
  ) where

import Data.Bool (Bool, (||))
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Ord (Ord)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import RealWorld.Conduit.Articles.Database.Article (Article)
import RealWorld.Conduit.Users.Database.User (User)
import Text.Show (Show)

data Decorated = Decorated
  { article :: Article
  , author :: User
  , tagList :: [Text]
  , favoriteCount :: Int
  , favorited :: Bool
  } deriving (Show, Eq, Ord)

instance Semigroup Decorated where
  Decorated _ _ tagNamesA _ favdA <> Decorated artB userB tagNamesB favsB favdB =
    Decorated artB userB (tagNamesA <> tagNamesB) favsB (favdA || favdB)
