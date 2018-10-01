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
import qualified Data.Set as Set
import Data.String (String)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types
  ( hAuthorization
  , status200
  , status201
  , status404
  , status422
  )
import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleBody, simpleStatus))
import RealWorld.Conduit.Articles.Web (Articles)
import qualified RealWorld.Conduit.Articles.Web as Articles
import RealWorld.Conduit.Articles.Web.Article (Article)
import qualified RealWorld.Conduit.Articles.Web.Article as Article
import RealWorld.Conduit.Articles.Web.Article.Attributes
  ( Attributes(Attributes)
  )
import qualified RealWorld.Conduit.Articles.Web.Article.Attributes as Attributes
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
import Test.Hspec.Wai.Extended (WaiSession, post', put', delete', get')
import Test.Hspec.Wai.JSON (json)

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

create :: Account -> Attributes.Create -> WaiSession (Either String Article)
create account =
  (articleFromResponse <$>) .
  post'
    "/api/articles"
    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))] .
  encode . articleNamespace

createParams :: Attributes.Create
createParams =
  Attributes
    { Attributes.title = "Title"
    , Attributes.description = "Description."
    , Attributes.body = "Body."
    , Attributes.tagList = Set.empty
    }

spec :: Spec
spec =
  around (withApp app) $ do
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
            Article.slug <$>
              (articleFromResponse =<< res) `shouldBe` Right "my-cool-thing"
            Article.title <$>
              (articleFromResponse =<< res) `shouldBe` Right "My cool thing"
            Article.description <$>
              (articleFromResponse =<< res) `shouldBe` Right "Yet more about it."
            Article.body <$>
              (articleFromResponse =<< res) `shouldBe` Right "The whole kit and kaboodle."

      context "when provided a valid body with invalid values" $
        it "responds with 422 and a json encoded error response" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              void $ ExceptT $ create account createParams { Attributes.title = "Taken" }
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

    describe "GET /api/articles/:slug" $ do
      context "when article doesn't exist" $
        it "responds with a 404" $ do
          res <- get' "/api/articles/nah" []
          liftIO $ simpleStatus res `shouldBe` status404

      context "when article does exist" $
        it "responds with 200 and a json encoded article response" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              article <-
                ExceptT $
                create
                  account
                  Attributes
                    { Attributes.title = "Title"
                    , Attributes.description = "Description."
                    , Attributes.body = "Body."
                    , Attributes.tagList = Set.empty
                    }
              lift $ get' (encodeUtf8 ("/api/articles/" <> Article.slug article)) []
          liftIO $ do
            simpleStatus <$> res `shouldBe` Right status200
            Article.slug <$>
              (articleFromResponse =<< res) `shouldBe` Right "title"
            Article.title <$>
              (articleFromResponse =<< res) `shouldBe` Right "Title"
            Article.description <$>
              (articleFromResponse =<< res) `shouldBe` Right "Description."
            Article.body <$>
              (articleFromResponse =<< res) `shouldBe` Right "Body."

    describe "PUT /api/articles/:slug" $ do
      context "when article doesn't exist" $
        it "responds with a 404" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                put'
                  "/api/articles/nah"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                      article: {
                        title: "Updated title"
                      }
                    }|]
          liftIO $ simpleStatus <$> res `shouldBe` Right status404

      context "when article does exist" $ do
        context "when provided a valid body with valid values" $
          it "responds with 200 and a json encoded article response" $ do
            res <-
              runExceptT $ do
                account <-
                  ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
                article <-
                  ExceptT $
                  create account createParams {Attributes.title = "First title"}
                lift $
                  put'
                    (encodeUtf8 ("/api/articles/" <> Article.slug article))
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                    [json|{
                        article: {
                          title: "Updated title"
                        }
                      }|]
            liftIO $ do
              simpleStatus <$> res `shouldBe` Right status200
              Article.slug <$>
                (articleFromResponse =<< res) `shouldBe` Right "updated-title"
              Article.title <$>
                (articleFromResponse =<< res) `shouldBe` Right "Updated title"

        context "when provided a valid body with invalid values" $
          it "responds with 422 and a json encoded error response" $ do
            res <-
              runExceptT $ do
                account <-
                  ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
                article <-
                  ExceptT $
                  create account createParams {Attributes.title = "Not taken"}
                void $ ExceptT $ create account createParams { Attributes.title = "Taken" }
                lift $
                  put'
                    (encodeUtf8 ("/api/articles/" <> Article.slug article))
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                    [json|{
                        article: {
                          title: "Taken"
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

    describe "DELETE /api/articles/:slug" $ do
      context "when article doesn't exist" $
        it "responds with a 404" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                delete'
                  "/api/articles/nah"
                  [ ( hAuthorization
                    , encodeUtf8 ("Bearer " <> Account.token account))
                  ]
          liftIO $ simpleStatus <$> res `shouldBe` Right status404

      context "when article does exist" $
        it "responds with 200 and a json encoded article response" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              article <- ExceptT $ create account createParams
              lift $
                delete'
                  (encodeUtf8 ("/api/articles/" <> Article.slug article))
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
          liftIO $ simpleStatus <$> res `shouldBe` Right status200
