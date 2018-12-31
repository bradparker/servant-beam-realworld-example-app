module RealWorld.Conduit.Articles.Web.View
  ( handler
  , loadArticle
  , View
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (UserId)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (optionallyLoadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler))
import Servant.API ((:>), Capture, Get, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type View =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  Auth '[JWT] Claim :>
  Get '[JSON] (Namespace "article" Article)

handler ::
     Environment
  -> Text
  -> AuthResult Claim
  -> Handler (Namespace "article" Article)
handler environment slug authResult = do
  user <- optionallyLoadAuthorizedUser environment authResult
  Namespace <$> loadArticle environment (primaryKey <$> user) slug

loadArticle :: Environment -> Maybe UserId -> Text -> Handler Article
loadArticle environment currentUserId slug =
  withDatabaseConnection environment $
    Handler .
    maybeToExceptT (notFound "Article") .
    MaybeT . runReaderT (Database.find currentUserId slug)
