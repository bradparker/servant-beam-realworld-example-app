module RealWorld.Conduit.Articles.Web.Destroy
  ( handler
  , Destroy
  ) where

import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Environment (Environment(..))
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
     Environment
  -> Text
  -> AuthResult Claim
  -> Handler NoContent
handler environment slug authResult = do
  user <- loadAuthorizedUser environment authResult
  article <- loadArticleBySlug environment slug
  authorizeUserToDestroyArticle user article
  NoContent <$ destroyArticle environment article

authorizeUserToDestroyArticle :: User -> Persisted.Article -> Handler ()
authorizeUserToDestroyArticle user article =
  unless (Persisted.author article == primaryKey user) (throwError forbidden)

destroyArticle :: Environment -> Persisted.Article -> Handler ()
destroyArticle environment article =
  withDatabaseConnection environment $ \conn ->
    liftIO $ Database.destroy conn $ primaryKey article
