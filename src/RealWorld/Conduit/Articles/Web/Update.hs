module RealWorld.Conduit.Articles.Web.Update
  ( handler
  , Update
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Database.Article.Attributes (forUpdate)
import RealWorld.Conduit.Articles.Web.Article (Article)
import qualified RealWorld.Conduit.Articles.Web.Article.Attributes as Attributes
import RealWorld.Conduit.Articles.Web.Create (decorateArticle)
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (failedValidation, forbidden)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), throwError)
import Servant.API ((:>), Capture, JSON, Put, ReqBody)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Update =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  ReqBody '[JSON] (Namespace "article" Attributes.Update) :>
  Auth '[JWT] Claim :>
  Put '[JSON] (Namespace "article" Article)

handler ::
     Environment
  -> Text
  -> Namespace "article" Attributes.Update
  -> AuthResult Claim
  -> Handler (Namespace "article" Article)
handler environment slug (Namespace params) authResult = do
  user <- loadAuthorizedUser environment authResult
  article <- loadArticleBySlug environment slug
  authorizeUserToUpdateArticle user article
  updated <- updateArticle environment article params
  Namespace <$> decorateArticle environment (Just user) updated

authorizeUserToUpdateArticle :: User -> Persisted.Article -> Handler ()
authorizeUserToUpdateArticle user article =
  unless (Persisted.author article == primaryKey user) (throwError forbidden)

updateArticle ::
     Environment
  -> Persisted.Article
  -> Attributes.Update
  -> Handler Database.Article
updateArticle environment article params =
  withDatabaseConnection environment $ \conn -> do
    attributes <-
      Handler $
      withExceptT failedValidation $
      forUpdate
        conn
        article
        (Attributes.title params)
        (Attributes.description params)
        (Attributes.body params)
    liftIO $ do
      updated <- Database.update conn article attributes
      traverse_
        (Database.replaceTags conn (primaryKey article))
        (Attributes.tagList params)
      pure updated
