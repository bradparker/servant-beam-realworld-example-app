module RealWorld.Conduit.Articles.Web.Destroy
  ( handler
  , Destroy
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Text (Text)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.Article (Article, fromDecorated)
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (forbidden)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler, throwError)
import Servant.API ((:>), Capture, Delete, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Destroy =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  Auth '[JWT] Claim :>
  Delete '[JSON] (Namespace "article" Article)

handler ::
     Handle
  -> Text
  -> AuthResult Claim
  -> Handler (Namespace "article" Article)
handler handle slug authResult = do
  user <- loadAuthorizedUser handle authResult
  article <- loadArticleBySlug handle slug
  authorizeUserToDestroyArticle user article
  Namespace <$> destroyArticle handle user article

authorizeUserToDestroyArticle :: User -> Persisted.Article -> Handler ()
authorizeUserToDestroyArticle user article =
  unless (Persisted.author article == primaryKey user) (throwError forbidden)

destroyArticle ::
     Handle -> User -> Persisted.Article -> Handler Article
destroyArticle handle user article =
  withDatabaseConnection handle $ \conn ->
    liftIO $ do
      Database.destroy conn $ primaryKey article
      fromDecorated <$> Database.decorate conn user article
