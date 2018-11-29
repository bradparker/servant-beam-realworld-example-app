module RealWorld.Conduit.Comments.Database
  ( create
  , destroy
  , find
  , forArticle
  ) where

import Data.Time (getCurrentTime)
import Database.Beam
  ( (==.)
  , all_
  , default_
  , delete
  , filter_
  , insertExpressions
  , primaryKey
  , runDelete
  , runSelectReturningList
  , runSelectReturningOne
  , select
  , val_
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import Prelude hiding (find)
import RealWorld.Conduit.Articles.Database.Article (ArticleId)
import RealWorld.Conduit.Comments.Database.Comment
  ( Comment
  , CommentId
  , CommentT(Comment)
  )
import qualified RealWorld.Conduit.Comments.Database.Comment as Comment
import RealWorld.Conduit.Database
  ( ConduitDb(conduitComments, conduitUsers)
  , conduitDb
  , insertOne
  )
import RealWorld.Conduit.Users.Database.User (User, UserId)

create :: Connection -> UserId -> ArticleId -> Text -> IO Comment
create conn authorId articleId body = do
  currentTime <- getCurrentTime
  insertOne
    conn
    (conduitComments conduitDb)
    (insertExpressions
       [ Comment
           { Comment.id = default_
           , Comment.body = val_ body
           , Comment.article = val_ articleId
           , Comment.author = val_ authorId
           , Comment.createdAt = val_ currentTime
           , Comment.updatedAt = val_ currentTime
           }
       ])

destroy :: Connection -> CommentId -> IO ()
destroy conn comment =
  runBeamPostgres conn $
  runDelete $
  delete (conduitComments conduitDb) $ \candidate ->
    primaryKey candidate ==. val_ comment

find :: Connection -> CommentId -> IO (Maybe (User, Comment))
find conn commentId =
  runBeamPostgres conn $
  runSelectReturningOne $
  select $ do
    comment <-
      filter_
        ((val_ commentId ==.) . primaryKey)
        (all_ (conduitComments conduitDb))
    user <-
      filter_
        ((Comment.author comment ==.) . primaryKey)
        (all_ (conduitUsers conduitDb))
    pure (user, comment)

forArticle :: Connection -> ArticleId -> IO [(User, Comment)]
forArticle conn articleId =
  runBeamPostgres conn $
  runSelectReturningList $
  select $ do
    comment <-
      filter_
        ((val_ articleId ==.) . Comment.article)
        (all_ (conduitComments conduitDb))
    user <-
      filter_
        ((Comment.author comment ==.) . primaryKey)
        (all_ (conduitUsers conduitDb))
    pure (user, comment)
