module RealWorld.Conduit.Comments.Web
  ( server
  , Comments
  ) where

import qualified RealWorld.Conduit.Users.Profile as Profile
import qualified RealWorld.Conduit.Comments.Comment as Comment
import qualified RealWorld.Conduit.Users.Database.User as User
import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema)
import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Article as Article
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.View (loadArticle)
import RealWorld.Conduit.Comments.Comment (Comment)
import qualified RealWorld.Conduit.Comments.Database as Database
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (UserId)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (forbidden, internalServerError, notFound)
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

newtype CommentAttributes = CommentAttributes { body :: Text }

deriving instance Generic CommentAttributes
deriving instance FromJSON CommentAttributes
deriving instance ToJSON CommentAttributes
deriving instance ToSchema CommentAttributes

type Create =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  ReqBody '[JSON] (Namespace "comment" CommentAttributes) :>
  Auth '[JWT] Claim :>
  PostCreated '[JSON] (Namespace "comment" Comment)

create ::
     Environment
  -> Text
  -> Namespace "comment" CommentAttributes
  -> AuthResult Claim
  -> Handler (Namespace "comment" Comment)
create environment slug (Namespace params) authResult = Namespace <$> do
  user <- loadAuthorizedUser environment authResult
  article <- loadArticle environment (Just (primaryKey user)) slug
  Handler $ withDatabaseConnection environment $ \conn ->
    withExceptT (internalServerError . show) $
      usingReaderT conn $
      Database.create (primaryKey user) (Persisted.ArticleId (Article.id article)) (body params)

type Destroy =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  Capture "id" Int :>
  Auth '[JWT] Claim :>
  DeleteNoContent '[JSON] NoContent

loadComment :: Environment -> Maybe UserId -> Int -> Handler Comment
loadComment environment currentUserId identifier =
  withDatabaseConnection environment
    $ Handler
    . maybeToExceptT (notFound "Comment")
    . MaybeT
    . runReaderT (Database.find currentUserId identifier)

destroy :: Environment -> Text -> Int -> AuthResult Claim -> Handler NoContent
destroy environment slug identifier authResult = do
  user <- loadAuthorizedUser environment authResult
  void $ loadArticle environment (Just (primaryKey user)) slug
  comment <- loadComment environment (Just (primaryKey user)) identifier
  unless
    (Profile.username (Comment.author comment) == User.username user)
    (throwError forbidden)
  NoContent <$ destroyComment environment (Comment.id comment)

destroyComment :: Environment -> Int -> Handler ()
destroyComment environment comment =
  withDatabaseConnection environment $ runReaderT (Database.destroy comment)

type AllForArticle =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  Get '[JSON] (Namespace "comments" [Comment])

allForArticle :: Environment -> Text -> Handler (Namespace "comments" [Comment])
allForArticle environment slug =
  Namespace <$> do
    article <- loadArticle environment Nothing slug
    withDatabaseConnection environment $
      runReaderT $ Database.forArticle Nothing (Article.slug article)

type Comments = Create :<|> Destroy :<|> AllForArticle

server :: Environment -> Server Comments
server environment =
  create environment :<|>
  destroy environment :<|>
  allForArticle environment
