module RealWorld.Conduit.Database
  ( ConduitDb(..)
  , QueryError(..)
  , conduitDb
  , maybeRow
  , openConduitDb
  , rowList
  , singleRow
  ) where

import Control.Monad.Error.Class (MonadError, throwError)
import qualified Data.Conduit as Conduit
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.List as Conduit
import Database.Beam
  ( Database
  , DatabaseSettings
  , TableEntity
  , dbModification
  , defaultDbSettings
  , fieldNamed
  , modifyTable
  , tableModification
  , withDbModification
  )
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import RealWorld.Conduit.Articles.Database.Article (ArticleT)
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.ArticleTag (ArticleTagT)
import RealWorld.Conduit.Articles.Database.Favorite (FavoriteT)
import RealWorld.Conduit.Comments.Database.Comment (CommentT)
import qualified RealWorld.Conduit.Comments.Database.Comment as Comment
import RealWorld.Conduit.Tags.Database.Tag (TagT)
import RealWorld.Conduit.Users.Database.Follow (FollowT)
import RealWorld.Conduit.Users.Database.User (UserT)

data ConduitDb f = ConduitDb
  { conduitArticleTags :: f (TableEntity ArticleTagT)
  , conduitArticles :: f (TableEntity ArticleT)
  , conduitComments :: f (TableEntity CommentT)
  , conduitFavorites :: f (TableEntity FavoriteT)
  , conduitFollows :: f (TableEntity FollowT)
  , conduitTags :: f (TableEntity TagT)
  , conduitUsers :: f (TableEntity UserT)
  } deriving (Generic)

instance Database Postgres ConduitDb

newtype QueryError = UnexpectedAmountOfRows Int
  deriving Show

instance Exception QueryError

conduitDb :: DatabaseSettings Postgres ConduitDb
conduitDb =
  defaultDbSettings `withDbModification`
  dbModification
    { conduitArticles =
        modifyTable id $
        tableModification
          { Article.createdAt = fieldNamed "created_at"
          , Article.updatedAt = fieldNamed "updated_at"
          }
    , conduitComments =
        modifyTable id $
        tableModification
          { Comment.createdAt = fieldNamed "created_at"
          , Comment.updatedAt = fieldNamed "updated_at"
          }
    }

openConduitDb :: MonadIO m => ByteString -> m Connection
openConduitDb = liftIO . connectPostgreSQL

maybeRow :: Monad m => ConduitT () a m () -> m (Maybe a)
maybeRow c = Conduit.runConduit (c .| Conduit.await)

singleRow :: (MonadError QueryError m, Monad m) => ConduitT () a m () -> m a
singleRow c = maybe (throwError (UnexpectedAmountOfRows 0)) pure =<< maybeRow c

rowList :: Monad m => ConduitT () a m () -> m [a]
rowList c = Conduit.runConduit (c .| Conduit.consume)
