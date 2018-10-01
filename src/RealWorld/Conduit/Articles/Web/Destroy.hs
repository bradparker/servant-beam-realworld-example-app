module RealWorld.Conduit.Articles.Web.Destroy
  ( handler
  , Destroy
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Text (Text)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (forbidden)
import Servant (Handler, throwError)
import Servant.API ((:>), Capture, DeleteNoContent, JSON, NoContent(NoContent))
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Destroy =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  Auth '[JWT] Claim :>
  DeleteNoContent '[JSON] NoContent

handler ::
     Handle
  -> Text
  -> AuthResult Claim
  -> Handler NoContent
handler handle slug authResult = do
  user <- loadAuthorizedUser handle authResult
  article <- loadArticleBySlug handle slug
  authorizeUserToDestroyArticle user article
  NoContent <$ destroyArticle handle article

authorizeUserToDestroyArticle :: User -> Persisted.Article -> Handler ()
authorizeUserToDestroyArticle user article =
  unless (Persisted.author article == primaryKey user) (throwError forbidden)

destroyArticle :: Handle -> Persisted.Article -> Handler ()
destroyArticle handle article =
  withDatabaseConnection handle $ \conn ->
    liftIO $ Database.destroy conn $ primaryKey article
