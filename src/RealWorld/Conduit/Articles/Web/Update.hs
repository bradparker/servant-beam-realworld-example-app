module RealWorld.Conduit.Articles.Web.Update
  ( handler
  , loadArticle
  , Update
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Article as Article
import qualified RealWorld.Conduit.Articles.Article.Attributes as Attributes
import qualified RealWorld.Conduit.Articles.Database as Database
import RealWorld.Conduit.Articles.Web.View (loadArticle)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User, UserId)
import qualified RealWorld.Conduit.Users.Database.User as User
import qualified RealWorld.Conduit.Users.Profile as Profile
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors
  ( failedValidation
  , forbidden
  , internalServerError
  )
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
  article <- loadArticle environment (Just (primaryKey user)) slug
  authorizeUserToUpdateArticle user article
  Namespace <$> updateArticle environment (primaryKey user) article params

authorizeUserToUpdateArticle :: User -> Article -> Handler ()
authorizeUserToUpdateArticle user article =
  unless (Profile.username (Article.author article) == User.username user) (throwError forbidden)

updateArticle
  :: Environment -> UserId -> Article -> Attributes.Update -> Handler Article
updateArticle environment currentUserId article params =
  withDatabaseConnection environment $ \conn -> do
    attributes <-
      Handler
        $ withExceptT failedValidation
        $ usingReaderT conn
        $ Database.attributesForUpdate
            article
            (Attributes.title params)
            (Attributes.description params)
            (Attributes.body params)
            (Attributes.tagList params)
    Handler
      $ withExceptT (internalServerError . show)
      $ usingReaderT conn
      $ Database.update currentUserId (Article.slug article) attributes
