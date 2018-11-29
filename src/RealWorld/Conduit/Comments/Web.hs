module RealWorld.Conduit.Comments.Web
  ( server
  , Comments
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema)
import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Web.View (loadArticleBySlug)
import qualified RealWorld.Conduit.Comments.Database as Database
import qualified RealWorld.Conduit.Comments.Database.Comment as Persisted
import RealWorld.Conduit.Comments.Web.Comment (Comment, fromPersisted)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (forbidden, notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), Server, throwError)
import Servant.API
  ( (:<|>)((:<|>))
  , (:>)
  , Capture
  , DeleteNoContent
  , Get
  , JSON
  , NoContent(NoContent)
  , PostCreated
  , ReqBody
  )
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

newtype Attributes = Attributes { body :: Text }

deriving instance Generic Attributes
deriving instance FromJSON Attributes
deriving instance ToJSON Attributes
deriving instance ToSchema Attributes

type Create =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  ReqBody '[JSON] (Namespace "comment" Attributes) :>
  Auth '[JWT] Claim :>
  PostCreated '[JSON] (Namespace "comment" Comment)

create ::
     Environment
  -> Text
  -> Namespace "comment" Attributes
  -> AuthResult Claim
  -> Handler (Namespace "comment" Comment)
create environment slug (Namespace params) authResult = do
  user <- loadAuthorizedUser environment authResult
  article <- loadArticleBySlug environment slug
  withDatabaseConnection environment $ \conn ->
    liftIO $
    Namespace . fromPersisted user <$>
    Database.create conn (primaryKey user) (primaryKey article) (body params)

type Destroy =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  Capture "id" Int :>
  Auth '[JWT] Claim :>
  DeleteNoContent '[JSON] NoContent

loadCommentById :: Environment -> Int -> Handler (User, Persisted.Comment)
loadCommentById environment identifier =
  Handler $
  withDatabaseConnection environment $ \conn ->
    maybeToExceptT (notFound "Article") $
    MaybeT $ Database.find conn (Persisted.CommentId identifier)

destroy :: Environment -> Text -> Int -> AuthResult Claim -> Handler NoContent
destroy environment slug identifier authResult = do
  user <- loadAuthorizedUser environment authResult
  void $ loadArticleBySlug environment slug
  (author, comment) <- loadCommentById environment identifier
  unless (primaryKey author == primaryKey user) (throwError forbidden)
  NoContent <$ destroyComment environment comment

destroyComment :: Environment -> Persisted.Comment -> Handler ()
destroyComment environment comment =
  withDatabaseConnection environment $ \conn ->
    liftIO $ Database.destroy conn $ primaryKey comment

type AllForArticle =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  Get '[JSON] (Namespace "comments" [Comment])

allForArticle :: Environment -> Text -> Handler (Namespace "comments" [Comment])
allForArticle environment slug = do
  article <- loadArticleBySlug environment slug
  Handler $
    withDatabaseConnection environment $ \conn ->
      Namespace . (uncurry fromPersisted <$>) <$>
      liftIO (Database.forArticle conn (primaryKey article))

type Comments = Create :<|> Destroy :<|> AllForArticle

server :: Environment -> Server Comments
server environment =
  create environment :<|>
  destroy environment :<|>
  allForArticle environment
