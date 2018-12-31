module RealWorld.Conduit.Articles.Web.All
  ( handler
  , All
  ) where

import Prelude hiding (All)
import qualified Data.Set as Set
import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (optionallyLoadAuthorizedUser)
import Servant (Handler)
import Servant.API ((:>), Get, JSON, QueryParam, QueryParams)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import Database.Beam (primaryKey)

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
    (Articles <$> id <*> length) <$>
    usingReaderT
      conn
      (Database.all
         (primaryKey <$> user)
         (fromMaybe 20 limit)
         (fromMaybe 0 offset)
         (Set.fromList authors)
         (Set.fromList tags)
         (Set.fromList favorited))
