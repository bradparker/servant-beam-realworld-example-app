module RealWorld.Conduit.Articles.Web.Feed
  ( handler
  , Feed
  ) where

import Database.Beam.Postgres.Extended (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Articles.Web.Articles (Articles)
import qualified RealWorld.Conduit.Articles.Web.Articles as Articles
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import Servant (Handler)
import Servant.API ((:>), Get, JSON, QueryParam)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Feed =
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
    Articles.fromList <$>
      usingReaderT conn
        (Database.feed
          (primaryKey user)
          (fromMaybe 20 limit)
          (fromMaybe 0 offset))
