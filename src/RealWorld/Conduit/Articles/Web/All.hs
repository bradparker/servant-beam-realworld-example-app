module RealWorld.Conduit.Articles.Web.All
  ( handler
  , All
  ) where

import Prelude hiding (All)
import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Articles.Web.Article (fromDecorated)
import RealWorld.Conduit.Articles.Web.Articles (Articles(Articles))
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

handler ::
     Environment
  -> Maybe Integer
  -> Maybe Integer
  -> [Text]
  -> [Text]
  -> [Text]
  -> AuthResult Claim
  -> Handler Articles
handler environment limit offset tags authors favorited authResult = do
  let params =
        Database.QueryParams
          (fromMaybe 20 limit)
          (fromMaybe 0 offset)
          tags
          authors
          favorited
  user <- optionallyLoadAuthorizedUser environment authResult
  withDatabaseConnection environment $ \conn ->
    (Articles <$> id <*> length) . (fromDecorated <$>) <$>
    liftIO (Database.query conn user params)
