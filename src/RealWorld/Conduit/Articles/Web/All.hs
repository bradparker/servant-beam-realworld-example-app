module RealWorld.Conduit.Articles.Web.All
  ( handler
  , All
  ) where

import qualified Data.Set as Set
import Database.Beam (primaryKey)
import Prelude hiding (All)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Web.Articles as Articles
import RealWorld.Conduit.Articles.Web.Articles (Articles)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (optionallyLoadAuthorizedUser)
import Servant (Handler)
import Servant.API ((:>), Get, JSON, QueryParam, QueryParams)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type All =
  "api" :>
  "articles" :>
  QueryParam "limit" Integer :>
  QueryParam "offset" Integer :>
  QueryParams "tag" Text :>
  QueryParams "author" Text :>
  QueryParams "favorited" Text :>
  Auth '[JWT] Claim :>
  Get '[JSON] Articles

handler
  :: Environment
  -> Maybe Integer
  -> Maybe Integer
  -> [Text]
  -> [Text]
  -> [Text]
  -> AuthResult Claim
  -> Handler Articles
handler environment limit offset tags authors favorited authResult = do
  user <- optionallyLoadAuthorizedUser environment authResult
  withDatabaseConnection environment $ \conn ->
    Articles.fromList <$>
      usingReaderT conn
        (Database.all
           (primaryKey <$> user)
           (fromMaybe 20 limit)
           (fromMaybe 0 offset)
           (Set.fromList authors)
           (Set.fromList tags)
           (Set.fromList favorited))
