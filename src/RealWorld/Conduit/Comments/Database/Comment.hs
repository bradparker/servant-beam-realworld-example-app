module RealWorld.Conduit.Comments.Database.Comment
  ( CommentT(..)
  , Comment
  , CommentId
  , PrimaryKey(..)
  ) where

import Data.Time (UTCTime)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table(..))
import Prelude hiding (id)
import RealWorld.Conduit.Articles.Database.Article (ArticleT)
import RealWorld.Conduit.Users.Database.User (UserT)

data CommentT f = Comment
  { id :: Columnar f Int
  , createdAt :: Columnar f UTCTime
  , updatedAt :: Columnar f UTCTime
  , body :: Columnar f Text
  , article :: PrimaryKey ArticleT f
  , author :: PrimaryKey UserT f
  }

deriving instance Generic (CommentT f)
deriving instance Beamable CommentT

type Comment = CommentT Identity

deriving instance Show Comment

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId
    { unCommentId :: Columnar f Int
    }
  primaryKey = CommentId . id

deriving instance Generic (PrimaryKey CommentT f)
deriving instance Beamable (PrimaryKey CommentT)

type CommentId = PrimaryKey CommentT Identity
