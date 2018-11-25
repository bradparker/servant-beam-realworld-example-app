module RealWorld.Conduit.Articles.Database.Article
  ( ArticleT(..)
  , Article
  , ArticleId
  , PrimaryKey(..)
  ) where

import Prelude hiding (id)
import Data.Time (UTCTime)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table(..))
import RealWorld.Conduit.Users.Database.User (UserT)

data ArticleT f = Article
  { id :: Columnar f Int
  , slug :: Columnar f Text
  , title :: Columnar f Text
  , description :: Columnar f Text
  , body :: Columnar f Text
  , createdAt :: Columnar f UTCTime
  , updatedAt :: Columnar f UTCTime
  , author :: PrimaryKey UserT f
  }

deriving instance Generic (ArticleT f)
deriving instance Beamable ArticleT

type Article = ArticleT Identity

deriving instance Show Article
deriving instance Eq Article
deriving instance Ord Article

instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId
    { unArticleId :: Columnar f Int
    }
  primaryKey = ArticleId . id

deriving instance Generic (PrimaryKey ArticleT f)
deriving instance Beamable (PrimaryKey ArticleT)

type ArticleId = PrimaryKey ArticleT Identity

deriving instance Show ArticleId
deriving instance Eq ArticleId
deriving instance Ord ArticleId
