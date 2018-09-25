module RealWorld.Conduit.Articles.Database.Commands
  ( create
  , update
  , destroy
  ) where

import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe, catMaybes, fromMaybe, listToMaybe)
import Data.Time (getCurrentTime)
import Database.Beam
  ( (<-.)
  , (==.)
  , default_
  , delete
  , insertExpressions
  , primaryKey
  , runDelete
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.Article
  ( Article
  , ArticleId
  , ArticleT(Article)
  )
import RealWorld.Conduit.Articles.Database.Article.Attributes (Attributes(..))
import RealWorld.Conduit.Database (ConduitDb(..), conduitDb, insertOne)
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
update conn currentArticle Attributes{slug, title, description, body} =
  fromMaybe currentArticle . listToMaybe <$>
  runBeamPostgres
    conn
    (runUpdateReturningList
       (conduitArticles conduitDb)
       (\article ->
          catMaybes
            [ (Article.slug article <-.) . val_ <$> slug
            , (Article.title article <-.) . val_ <$> title
            , (Article.description article <-.) . val_ <$> description
            , (Article.body article <-.) . val_ <$> body
            ])
       (\article -> primaryKey article ==. val_ (primaryKey currentArticle)))

destroy :: Connection -> ArticleId -> IO ()
destroy conn article =
  runBeamPostgres conn $
  runDelete $
  delete (conduitArticles conduitDb) $ \candidate ->
    primaryKey candidate ==. val_ article
