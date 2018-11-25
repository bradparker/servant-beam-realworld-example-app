module RealWorld.Conduit.Articles.Web.View
  ( handler
  , loadArticleBySlug
  , View
  ) where

import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.Article (Article)
import RealWorld.Conduit.Articles.Web.Create (decorateArticle)
import RealWorld.Conduit.Environment (Environment(..))
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
  article <- loadArticleBySlug environment slug
  Namespace <$> decorateArticle environment user article

loadArticleBySlug :: Environment -> Text -> Handler Persisted.Article
loadArticleBySlug environment slug =
  withDatabaseConnection environment $ \conn ->
    Handler $
    maybeToExceptT (notFound "Article") $
    MaybeT $ Database.findBySlug conn slug
