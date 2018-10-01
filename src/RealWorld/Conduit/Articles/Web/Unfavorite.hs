module RealWorld.Conduit.Articles.Web.Unfavorite
  ( handler
  , Unfavorite
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Functor ((<$>), void)
import Data.Text (Text)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Database
import RealWorld.Conduit.Articles.Web.Article (Article, fromDecorated)
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import RealWorld.Conduit.Handle (Handle(..))
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

handler :: Handle -> AuthResult Claim -> Text -> Handler (Namespace "article" Article)
handler handle authResult slug = do
  user <- loadAuthorizedUser handle authResult
  article <- loadArticleBySlug handle slug
  unfavoriteArticle handle user article
  withDatabaseConnection handle $ \conn ->
    Namespace . fromDecorated <$> liftIO (Database.decorate conn user article)

unfavoriteArticle :: Handle -> User -> Database.Article -> Handler ()
unfavoriteArticle handle user article =
  withDatabaseConnection handle $ \conn ->
    liftIO $ void $ Database.unfavorite conn (primaryKey article) (primaryKey user)
