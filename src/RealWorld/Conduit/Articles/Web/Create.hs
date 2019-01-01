module RealWorld.Conduit.Articles.Web.Create
  ( handler
  , Create
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Article (Article)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Web.Article.Attributes as Attributes
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (failedValidation, internalServerError)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler))
import Servant.API ((:>), JSON, PostCreated, ReqBody)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Create =
  "api" :>
  "articles" :>
  Auth '[JWT] Claim :>
  ReqBody '[JSON] (Namespace "article" Attributes.Create) :>
  PostCreated '[JSON] (Namespace "article" Article)

handler ::
     Environment
  -> AuthResult Claim
  -> Namespace "article" Attributes.Create
  -> Handler (Namespace "article" Article)
handler environment authresult (Namespace params) = do
  user <- loadAuthorizedUser environment authresult
  Namespace <$> createArticle environment user params

createArticle :: Environment -> User -> Attributes.Create -> Handler Article
createArticle environment user params =
  Handler $
  withDatabaseConnection environment $ \conn -> do
    attributes <-
      withExceptT failedValidation $
      usingReaderT conn $
      Database.attributesForInsert
        (Attributes.title params)
        (Attributes.description params)
        (Attributes.body params)
        (Attributes.tagList params)
    withExceptT (internalServerError . show) $
      usingReaderT conn $
      Database.create (primaryKey user) attributes
