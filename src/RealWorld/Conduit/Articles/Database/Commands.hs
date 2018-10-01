module RealWorld.Conduit.Articles.Database.Commands
  ( assignTags
  , create
  , destroy
  , favorite
  , replaceTags
  , unfavorite
  , update
  ) where

import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.List (map)
import Data.Maybe (Maybe(Just), catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Beam
  ( (&&.)
  , (<-.)
  , (==.)
  , default_
  , delete
  , insertExpressions
  , insertValues
  , primaryKey
  , runDelete
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Conduit (runInsert)
import Database.Beam.Postgres.Full
  ( conflictingFields
  , insert
  , onConflict
  , onConflictDoNothing
  )
import Database.PostgreSQL.Simple (Connection)
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.Article
  ( Article
  , ArticleId
  , ArticleT(Article)
  )
import RealWorld.Conduit.Articles.Database.Article.Attributes (Attributes(..))
import RealWorld.Conduit.Articles.Database.ArticleTag (ArticleTagT(ArticleTag))
import qualified RealWorld.Conduit.Articles.Database.ArticleTag as ArticleTag
import RealWorld.Conduit.Articles.Database.Favorite (Favorite, FavoriteT(..))
import RealWorld.Conduit.Database (ConduitDb(..), conduitDb, insertOne)
import qualified RealWorld.Conduit.Tags.Database as Tag
import RealWorld.Conduit.Users.Database.User (UserId)
import System.IO (IO)

create :: Connection -> UserId -> Attributes Identity -> IO Article
create conn authorId Attributes {slug, title, description, body} = do
  currentTime <- getCurrentTime
  insertOne conn (conduitArticles conduitDb) $
    insertExpressions
      [ Article
          { Article.id = default_
          , Article.slug = val_ slug
          , Article.title = val_ title
          , Article.description = val_ description
          , Article.body = val_ body
          , Article.createdAt = val_ currentTime
          , Article.updatedAt = val_ currentTime
          , Article.author = val_ authorId
          }
      ]

update :: Connection -> Article -> Attributes Maybe -> IO Article
update conn currentArticle Attributes {slug, title, description, body} =
  fromMaybe currentArticle . listToMaybe <$> do
    currentTime <- getCurrentTime
    runBeamPostgres conn $
      runUpdateReturningList
        (conduitArticles conduitDb)
        (\article ->
           catMaybes
             [ (Article.slug article <-.) . val_ <$> slug
             , (Article.title article <-.) . val_ <$> title
             , (Article.description article <-.) . val_ <$> description
             , (Article.body article <-.) . val_ <$> body
             , Just (Article.updatedAt article <-. val_ currentTime)
             ])
        ((val_ (primaryKey currentArticle) ==.) . primaryKey)

assignTags :: Connection -> ArticleId -> Set Text -> IO ()
assignTags conn article tagNames = do
  tagIds <- map primaryKey <$> Tag.create conn tagNames
  void $
    runInsert conn $
    insert
      (conduitArticleTags conduitDb)
      (insertValues (map (ArticleTag article) tagIds))
      (onConflict (conflictingFields id) onConflictDoNothing)

deleteTags :: Connection -> ArticleId -> IO ()
deleteTags conn article =
  runBeamPostgres conn $
  runDelete $
  delete
    (conduitArticleTags conduitDb)
    ((val_ article ==.) . ArticleTag.article)

replaceTags :: Connection -> ArticleId -> Set Text -> IO ()
replaceTags conn article tagList =
  deleteTags conn article *> assignTags conn article tagList

destroy :: Connection -> ArticleId -> IO ()
destroy conn article =
  runBeamPostgres conn $
  runDelete $
  delete (conduitArticles conduitDb) ((val_ article ==.) . primaryKey)

favorite :: Connection -> ArticleId -> UserId -> IO Favorite
favorite conn article user =
  insertOne
    conn
    (conduitFavorites conduitDb)
    (insertValues [Favorite article user])

unfavorite :: Connection -> ArticleId -> UserId -> IO ()
unfavorite conn article user =
  runBeamPostgres conn $
  runDelete $
  delete (conduitFavorites conduitDb) $ \(Favorite favArticle favUser) ->
    favUser ==. val_ user &&. favArticle ==. val_ article
