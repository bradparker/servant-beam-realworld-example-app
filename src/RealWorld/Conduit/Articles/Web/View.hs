module RealWorld.Conduit.Articles.Web.View
  ( handler
  , loadArticleBySlug
  , View
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Text (Text)
import qualified RealWorld.Conduit.Articles.Database as Database
import qualified RealWorld.Conduit.Articles.Database.Article as Persisted
import RealWorld.Conduit.Articles.Web.Article (Article, fromDecorated)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Web.Errors (notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler))
import Servant.API ((:>), Capture, Get, JSON)

type View =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  Get '[JSON] (Namespace "article" Article)

handler ::
     Handle
  -> Text
  -> Handler (Namespace "article" Article)
handler handle slug = do
  article <- loadArticleBySlug handle slug
  author <- loadAuthor handle article
  withDatabaseConnection handle $ \conn ->
    Namespace . fromDecorated <$> liftIO (Database.decorate conn author article)

loadAuthor :: Handle -> Persisted.Article -> Handler User
loadAuthor handle article =
  withDatabaseConnection handle $ \conn ->
    Handler $
    maybeToExceptT (notFound "Author") $
    MaybeT $ liftIO $ Users.find conn (Persisted.author article)

loadArticleBySlug :: Handle -> Text -> Handler Persisted.Article
loadArticleBySlug handle slug =
  withDatabaseConnection handle $ \conn ->
    Handler $
    maybeToExceptT (notFound "Article") $
    MaybeT $ Database.findBySlug conn slug
