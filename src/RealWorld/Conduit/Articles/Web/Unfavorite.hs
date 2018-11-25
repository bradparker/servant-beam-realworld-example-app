module RealWorld.Conduit.Articles.Web.Unfavorite
  ( handler
  , Unfavorite
  ) where

import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Database
import RealWorld.Conduit.Articles.Web.Article (Article)
import RealWorld.Conduit.Articles.Web.Create (decorateArticle)
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler)
import Servant.API ((:>), Capture, Delete, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Unfavorite =
  Auth '[JWT] Claim :>
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "favorite" :>
  Delete '[JSON] (Namespace "article" Article)

handler :: Environment -> AuthResult Claim -> Text -> Handler (Namespace "article" Article)
handler environment authResult slug = do
  user <- loadAuthorizedUser environment authResult
  article <- loadArticleBySlug environment slug
  unfavoriteArticle environment user article
  Namespace <$> decorateArticle environment (Just user) article

unfavoriteArticle :: Environment -> User -> Database.Article -> Handler ()
unfavoriteArticle environment user article =
  withDatabaseConnection environment $ \conn ->
    liftIO $ void $ Database.unfavorite conn (primaryKey article) (primaryKey user)
