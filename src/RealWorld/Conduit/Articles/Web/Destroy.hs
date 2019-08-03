module RealWorld.Conduit.Articles.Web.Destroy
  ( handler
  , Destroy
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Article (Article)
import RealWorld.Conduit.Articles.Article as Article
import RealWorld.Conduit.Users.Profile as Profile
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.View (loadArticle)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (forbidden)
import Servant (Handler, throwError)
import Servant.API ((:>), Capture, DeleteNoContent, JSON, NoContent(NoContent))
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Destroy =
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
  article <- loadArticle environment (Just (primaryKey user)) slug
  authorizeUserToDestroyArticle user article
  NoContent <$ destroyArticle environment article

authorizeUserToDestroyArticle :: User -> Article -> Handler ()
authorizeUserToDestroyArticle user article =
  unless (Profile.username (Article.author article) == User.username user) (throwError forbidden)

destroyArticle :: Environment -> Article -> Handler ()
destroyArticle environment article =
  withDatabaseConnection environment $ \conn ->
    usingReaderT conn $
    Database.destroy $ Persisted.ArticleId $ Article.id article
