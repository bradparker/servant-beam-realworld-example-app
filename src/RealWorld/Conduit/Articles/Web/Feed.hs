module RealWorld.Conduit.Articles.Web.Feed
  ( handler
  , Feed
  ) where

import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import Servant (Handler)
import Servant.API ((:>), Get, JSON, QueryParam)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import Database.Beam.Postgres.Extended (primaryKey)

import RealWorld.Conduit.Articles.Article (Article)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema)

data Articles = Articles
  { articles :: [Article]
  , articlesCount :: Int
  }

deriving instance Generic Articles
deriving instance ToJSON Articles
deriving instance ToSchema Articles
deriving instance FromJSON Articles

type Feed =
  "api" :>
  "articles" :>
  "feed" :>
  QueryParam "limit" Integer :>
  QueryParam "offset" Integer :>
  Auth '[JWT] Claim :>
  Get '[JSON] Articles

handler ::
     Environment
  -> Maybe Integer
  -> Maybe Integer
  -> AuthResult Claim
  -> Handler Articles
handler environment limit offset authResult = do
  user <- loadAuthorizedUser environment authResult
  withDatabaseConnection environment $ \conn ->
    (Articles <$> id <*> length) <$>
    usingReaderT conn (Database.feed (primaryKey user) (fromMaybe 20 limit) (fromMaybe 0 offset))
