module RealWorld.Conduit.Articles.Web.Create
  ( handler
  , decorateArticle
  , Create
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Database
import RealWorld.Conduit.Articles.Database.Article.Attributes (forInsert)
import RealWorld.Conduit.Articles.Web.Article (Article, fromDecorated)
import qualified RealWorld.Conduit.Articles.Web.Article.Attributes as Attributes
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (failedValidation, notFound)
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
  article <- createArticle environment user params
  Namespace <$> decorateArticle environment (Just user) article

decorateArticle :: Environment -> Maybe User -> Database.Article -> Handler Article
decorateArticle environment currentUser article =
  Handler $
  withDatabaseConnection environment $ \conn ->
    fromDecorated <$>
    maybeToExceptT
      (notFound "Article")
      (MaybeT (Database.decorate conn currentUser article))

createArticle :: Environment -> User -> Attributes.Create -> Handler Database.Article
createArticle environment user params =
  Handler $
  withDatabaseConnection environment $ \conn -> do
    attributes <-
      withExceptT failedValidation $
      forInsert
        conn
        (Attributes.title params)
        (Attributes.description params)
        (Attributes.body params)
    lift $ do
      article <- Database.create conn (primaryKey user) attributes
      Database.assignTags conn (primaryKey article) (Attributes.tagList params)
      pure article
