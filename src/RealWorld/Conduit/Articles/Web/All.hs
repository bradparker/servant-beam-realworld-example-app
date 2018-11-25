module RealWorld.Conduit.Articles.Web.All
  ( handler
  , All
  ) where

import Control.Applicative ((<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (length)
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Maybe (Maybe, fromMaybe)
import Data.Text (Text)
import Prelude (Integer)
import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Articles.Web.Article (fromDecorated)
import RealWorld.Conduit.Articles.Web.Articles (Articles(Articles))
import RealWorld.Conduit.Handle (Handle(..))
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
     Handle
  -> Maybe Integer
  -> Maybe Integer
  -> [Text]
  -> [Text]
  -> [Text]
  -> AuthResult Claim
  -> Handler Articles
handler handle limit offset tags authors favorited authResult = do
  let params =
        Database.QueryParams
          (fromMaybe 20 limit)
          (fromMaybe 0 offset)
          tags
          authors
          favorited
  user <- optionallyLoadAuthorizedUser handle authResult
  withDatabaseConnection handle $ \conn ->
    (Articles <$> id <*> length) . (fromDecorated <$>) <$>
    liftIO (Database.query conn user params)
