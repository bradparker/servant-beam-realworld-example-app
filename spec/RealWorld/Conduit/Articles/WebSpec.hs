module RealWorld.Conduit.Articles.WebSpec
  ( spec
  ) where

import Control.Monad ((=<<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either(Right))
import Data.Function (($), (.))
import Data.Functor ((<$>), void)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (hAuthorization, status201, status422)
import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleBody, simpleStatus))
import RealWorld.Conduit.Articles.Web (Articles)
import qualified RealWorld.Conduit.Articles.Web as Articles
import RealWorld.Conduit.Articles.Web.Article (Article)
import qualified RealWorld.Conduit.Articles.Web.Article as Article
import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Spec.Web (withApp)
import qualified RealWorld.Conduit.Users.Web as Users
import RealWorld.Conduit.Users.Web (Users)
import RealWorld.Conduit.Users.Web.Account (Account)
import qualified RealWorld.Conduit.Users.Web.Account as Account
import RealWorld.Conduit.Users.Web.Register (Registrant(Registrant))
import RealWorld.Conduit.Users.WebSpec (register)
import qualified RealWorld.Conduit.Web as Web
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace), unNamespace)
import Servant ((:<|>)((:<|>)), serveWithContext)
import Test.Hspec (Spec, around, context, describe, it, shouldBe)
import Test.Hspec.Wai.Extended (post', WaiSession)
import Test.Hspec.Wai.JSON (json)
import RealWorld.Conduit.Articles.Web.Create (ArticleCreate(..))
import qualified Data.Set as Set

type ArticlesAndUsers = Articles :<|> Users

articlesAndUsers :: Proxy ArticlesAndUsers
articlesAndUsers = Proxy

app :: Handle -> Application
app handle =
  serveWithContext
    articlesAndUsers
    (Web.context handle)
    (Articles.server handle :<|> Users.server handle)

decodeArticleNamespace ::
     FromJSON a => ByteString -> Either String (Namespace "article" a)
decodeArticleNamespace = eitherDecode

articleFromResponse :: SResponse -> Either String Article
articleFromResponse = (unNamespace <$>) . decodeArticleNamespace . simpleBody

articleNamespace :: a -> Namespace "article" a
articleNamespace = Namespace

create :: Account -> ArticleCreate -> WaiSession (Either String Article)
create account =
  (articleFromResponse <$>) .
  post'
    "/api/articles"
    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))] .
  encode . articleNamespace


createParams :: ArticleCreate
createParams =
  ArticleCreate
    { title = "Title"
    , description = "Description."
    , body = "Body."
    , tagList = Set.empty
    }

spec :: Spec
spec =
  around (withApp app) $
    describe "POST /api/articles" $ do
      context "when provided a valid body with valid values" $
        it "responds with 201 and a json encoded article response" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                post'
                  "/api/articles"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                    article: {
                      title: "My cool thing",
                      description: "Yet more about it.",
                      body: "The whole kit and kaboodle.",
                      tagList: []
                    }
                  }|]
          liftIO $ do
            simpleStatus <$> res `shouldBe` Right status201
            Article.slug <$> (articleFromResponse =<< res) `shouldBe` Right "my-cool-thing"
            Article.title <$> (articleFromResponse =<< res) `shouldBe` Right "My cool thing"
            Article.description <$> (articleFromResponse =<< res) `shouldBe` Right "Yet more about it."
            Article.body <$> (articleFromResponse =<< res) `shouldBe` Right "The whole kit and kaboodle."

      context "when provided a valid body with invalid values" $
        it "responds with 422 and a json encoded error response" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              void $ ExceptT $ create account createParams { title = "Taken" }
              lift $
                post'
                  "/api/articles"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                    article: {
                      title: "Taken",
                      description: "Desc.",
                      body: "Body",
                      tagList: []
                    }
                  }|]
          liftIO $ do
            simpleStatus <$> res `shouldBe` Right status422
            simpleBody <$> res `shouldBe` Right
              [json|{
                message: "Failed validation",
                errors: [
                  {
                    tag: "TitleWouldProduceDuplicateSlug",
                    contents: "taken"
                  }
                ]
              }|]
