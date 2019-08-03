module RealWorld.Conduit.Articles.Web.Favorite
  ( handler
  , Favorite
  ) where

import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Article as Article
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.View (loadArticle)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler)
import Servant.API ((:>), Capture, JSON, Post)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Favorite =
  Capture "slug" Text :>
  "favorite" :>
  Auth '[JWT] Claim :>
  Post '[JSON] (Namespace "article" Article)

handler :: Environment -> Text -> AuthResult Claim -> Handler (Namespace "article" Article)
handler environment slug authResult = do
  user <- loadAuthorizedUser environment authResult
  article <- loadArticle environment (Just (primaryKey user)) slug
  favoriteArticle environment user article
  Namespace <$> loadArticle environment (Just (primaryKey user)) slug

favoriteArticle :: Environment -> User -> Article -> Handler ()
favoriteArticle environment user article =
  withDatabaseConnection environment $ \conn ->
    void $ usingReaderT conn $ Database.favorite
      (Persisted.ArticleId (Article.id article))
      (primaryKey user)
