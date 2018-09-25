module RealWorld.Conduit.Articles.Database.Queries
  ( decorate
  , findBySlug
  , findByTitle
  ) where

import Control.Applicative (pure)
import Data.Bool (Bool(False))
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Text (Text)
import Database.Beam (all_)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Articles.Database.Article (Article)
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.Decorated (Decorated(..))
import RealWorld.Conduit.Database (ConduitDb(..), conduitDb, findBy)
import RealWorld.Conduit.Users.Database.User (User)
import System.IO (IO)

findByTitle :: Connection -> Text -> IO (Maybe Article)
findByTitle conn = findBy conn (all_ (conduitArticles conduitDb)) Article.title

findBySlug :: Connection -> Text -> IO (Maybe Article)
findBySlug conn = findBy conn (all_ (conduitArticles conduitDb)) Article.slug

decorate :: Connection -> User -> Article -> IO Decorated
decorate _ user article = pure $ Decorated article user [] 0 False
