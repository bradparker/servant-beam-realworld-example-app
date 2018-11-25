module RealWorld.Conduit.Articles.Web.View
  ( handler
  , loadArticleBySlug
  , View
  ) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Text (Text)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.Article (Article)
import RealWorld.Conduit.Articles.Web.Create (decorateArticle)
import RealWorld.Conduit.Handle (Handle(..))
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
     Handle
  -> Text
  -> AuthResult Claim
  -> Handler (Namespace "article" Article)
handler handle slug authResult = do
  user <- optionallyLoadAuthorizedUser handle authResult
  article <- loadArticleBySlug handle slug
  Namespace <$> decorateArticle handle user article

loadArticleBySlug :: Handle -> Text -> Handler Persisted.Article
loadArticleBySlug handle slug =
  withDatabaseConnection handle $ \conn ->
    Handler $
    maybeToExceptT (notFound "Article") $
    MaybeT $ Database.findBySlug conn slug
