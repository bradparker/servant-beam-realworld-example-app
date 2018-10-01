module RealWorld.Conduit.Articles.Web.Favorite
  ( handler
  , Favorite
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.Maybe (Maybe(Just))
import Data.Text (Text)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Database
import RealWorld.Conduit.Articles.Web.Article (Article)
import RealWorld.Conduit.Articles.Web.Create (decorateArticle)
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler)
import Servant.API ((:>), Capture, JSON, Post)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Favorite =
  Auth '[JWT] Claim :>
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "favorite" :>
  Post '[JSON] (Namespace "article" Article)

handler :: Handle -> AuthResult Claim -> Text -> Handler (Namespace "article" Article)
handler handle authResult slug = do
  user <- loadAuthorizedUser handle authResult
  article <- loadArticleBySlug handle slug
  favoriteArticle handle user article
  Namespace <$> decorateArticle handle (Just user) article

favoriteArticle :: Handle -> User -> Database.Article -> Handler ()
favoriteArticle handle user article =
  withDatabaseConnection handle $ \conn ->
    liftIO $
    void $ Database.favorite conn (primaryKey article) (primaryKey user)
