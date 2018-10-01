module RealWorld.Conduit.Articles.Web.Update
  ( handler
  , Update
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (withExceptT)
import Data.Eq ((==))
import Data.Foldable (traverse_)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Text (Text)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Database.Article.Attributes (forUpdate)
import RealWorld.Conduit.Articles.Web.Article (Article, fromDecorated)
import qualified RealWorld.Conduit.Articles.Web.Article.Attributes as Attributes
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Handle (Handle(..))
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
     Handle
  -> Text
  -> Namespace "article" Attributes.Update
  -> AuthResult Claim
  -> Handler (Namespace "article" Article)
handler handle slug (Namespace params) authResult = do
  user <- loadAuthorizedUser handle authResult
  article <- loadArticleBySlug handle slug
  authorizeUserToUpdateArticle user article
  Namespace <$> updateArticle handle user article params

authorizeUserToUpdateArticle :: User -> Persisted.Article -> Handler ()
authorizeUserToUpdateArticle user article =
  unless (Persisted.author article == primaryKey user) (throwError forbidden)

updateArticle ::
     Handle
  -> User
  -> Persisted.Article
  -> Attributes.Update
  -> Handler Article
updateArticle handle user article params =
  withDatabaseConnection handle $ \conn -> do
    attributes <- Handler $
      withExceptT failedValidation $
      forUpdate
        conn
        article
        (Attributes.title params)
        (Attributes.description params)
        (Attributes.body params)
    liftIO $ do
      updated <- Database.update conn article attributes
      traverse_ (Database.replaceTags conn (primaryKey article)) (Attributes.tagList params)
      fromDecorated <$> Database.decorate conn user updated
