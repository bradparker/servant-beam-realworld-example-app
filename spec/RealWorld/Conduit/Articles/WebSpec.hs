module RealWorld.Conduit.Articles.WebSpec
  ( spec
  ) where

import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Set as Set
import Data.Traversable (for)
import Network.HTTP.Types
  ( hAuthorization
  , status200
  , status201
  , status204
  , status404
  , status422
  )
import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleBody, simpleStatus))
import Prelude hiding (ByteString)
import RealWorld.Conduit.Articles.Web (Articles)
import qualified RealWorld.Conduit.Articles.Web as Articles
import RealWorld.Conduit.Articles.Web.Article (Article)
import qualified RealWorld.Conduit.Articles.Web.Article as Article
import RealWorld.Conduit.Articles.Web.Article.Attributes
  ( Attributes(Attributes)
  )
import qualified RealWorld.Conduit.Articles.Web.Article.Attributes as Attributes
import RealWorld.Conduit.Environment (Environment)
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
import Test.Hspec.Wai.Extended (WaiSession, delete', get', post', put')
import Test.Hspec.Wai.JSON (json)

type ArticlesAndUsers = Articles :<|> Users

articlesAndUsers :: Proxy ArticlesAndUsers
articlesAndUsers = Proxy

app :: Environment -> Application
app environment =
  serveWithContext
    articlesAndUsers
    (Web.context environment)
    (Articles.server environment :<|> Users.server environment)

decodeArticleNamespace ::
     FromJSON a => ByteString -> Either String (Namespace "article" a)
decodeArticleNamespace = eitherDecode

articleFromResponse :: SResponse -> Either String Article
articleFromResponse = (unNamespace <$>) . decodeArticleNamespace . simpleBody

articleNamespace :: a -> Namespace "article" a
articleNamespace = Namespace

create :: Account -> Attributes.Create -> ExceptT String WaiSession Article
create account =
  ExceptT . (articleFromResponse <$>) .
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

favorite :: Account -> Text -> ExceptT String WaiSession Article
favorite account slug =
  ExceptT $
  articleFromResponse <$>
  post'
    ("/api/articles/" <> encodeUtf8 slug <> "/favorite")
    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
    ""

decodeArticlesNamespace ::
     FromJSON a => ByteString -> Either String (Namespace "articles" a)
decodeArticlesNamespace = eitherDecode

articlesFromResponse :: SResponse -> Either String [Article]
articlesFromResponse = (unNamespace <$>) . decodeArticlesNamespace . simpleBody

createN ::
     Int
  -> Account
  -> (Int -> Attributes.Create)
  -> ExceptT String WaiSession [Article]
createN num account f = for [1 .. num] $ \n -> create account (f n)

defaultAccount :: ExceptT String WaiSession Account
defaultAccount = ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"

spec :: Spec
spec =
  around (withApp app) $ do
    describe "POST /api/articles" $ do
      context "when provided a valid body with valid values" $
        it "responds with 201 and a json encoded article response" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              lift $
                post'
                  "/api/articles"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                    article: {
                      title: "My cool thing",
                      description: "Yet more about it.",
                      body: "The whole kit and kaboodle.",
                      tagList: ["cats", "dogs"]
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
            Article.tagList <$>
              (articleFromResponse =<< res) `shouldBe` Right (Set.fromList ["cats", "dogs"])

      context "when provided a valid body with invalid values" $
        it "responds with 422 and a json encoded error response" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ create account createParams { Attributes.title = "Taken" }
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
              account <- defaultAccount
              article <-
                create
                  account
                  Attributes
                    { Attributes.title = "Title"
                    , Attributes.description = "Description."
                    , Attributes.body = "Body."
                    , Attributes.tagList = Set.fromList ["tag1", "tag2", "tag3"]
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
            Article.tagList <$>
              (articleFromResponse =<< res) `shouldBe` Right (Set.fromList ["tag1", "tag2", "tag3"])

    describe "PUT /api/articles/:slug" $ do
      context "when article doesn't exist" $
        it "responds with a 404" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              lift $
                put'
                  "/api/articles/nah"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                      article: {
                        title: "Updated title",
                        tagsList: ["updated", "tags"]
                      }
                    }|]
          liftIO $ simpleStatus <$> res `shouldBe` Right status404

      context "when article does exist" $ do
        context "when provided a valid body with valid values" $
          it "responds with 200 and a json encoded article response" $ do
            res <-
              runExceptT $ do
                account <- defaultAccount
                article <-
                  create account createParams {Attributes.title = "First title"}
                lift $
                  put'
                    (encodeUtf8 ("/api/articles/" <> Article.slug article))
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                    [json|{
                        article: {
                          title: "Updated title",
                          tagList: ["updated", "tags"]
                        }
                      }|]
            liftIO $ do
              simpleStatus <$> res `shouldBe` Right status200
              Article.slug <$>
                (articleFromResponse =<< res) `shouldBe` Right "updated-title"
              Article.title <$>
                (articleFromResponse =<< res) `shouldBe` Right "Updated title"
              Article.tagList <$>
                (articleFromResponse =<< res) `shouldBe`
                Right (Set.fromList ["updated", "tags"])

        context "when provided a valid body with invalid values" $
          it "responds with 422 and a json encoded error response" $ do
            res <-
              runExceptT $ do
                account <- defaultAccount
                article <-
                  create account createParams {Attributes.title = "Not taken"}
                void $ create account createParams { Attributes.title = "Taken" }
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
              account <- defaultAccount
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
              account <- defaultAccount
              article <- create account createParams
              lift $
                delete'
                  (encodeUtf8 ("/api/articles/" <> Article.slug article))
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
          liftIO $ simpleStatus <$> res `shouldBe` Right status204

    describe "GET /api/articles" $ do
      context "without a `limit` param" $
        it "responds with no more than 20 articles" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ createN 25 account $ \n ->
                createParams
                  { Attributes.title = "Title " <> show n
                  }
              lift $ get' "/api/articles/" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            length <$> articles `shouldBe` Right 20

      context "with a `limit` param" $
        it "responds with no more than that many articles" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ createN 15 account $ \n ->
                createParams
                  { Attributes.title = "Title " <> show n
                  }
              lift $ get' "/api/articles/?limit=10" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            length <$> articles `shouldBe` Right 10

      context "without an `offset` param" $
        it "responds with the first `limit` articles" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ createN 8 account $ \n ->
                createParams
                  { Attributes.title = "Article " <> show n
                  }
              lift $ get' "/api/articles/?limit=4" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "Article 1"
                , "Article 2"
                , "Article 3"
                , "Article 4"
                ]

      context "with an `offset` param" $
        it "responds with `limit` articles starting from `offset`" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ createN 8 account $ \n ->
                createParams
                  { Attributes.title = "Article " <> show n
                  }
              lift $ get' "/api/articles/?limit=4&offset=4" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "Article 5"
                , "Article 6"
                , "Article 7"
                , "Article 8"
                ]

      context "without a `tag` param" $
        it "responds with articles tagged with anything" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ createN 5 account $ \n ->
                createParams
                  { Attributes.title = "Tagged with foo " <> show n
                  , Attributes.tagList = Set.fromList ["foo"]
                  }
              void $ createN 5 account $ \n ->
                createParams
                  { Attributes.title = "Tagged with foo and bar " <> show n
                  , Attributes.tagList = Set.fromList ["foo", "bar"]
                  }
              lift $ get' "/api/articles/" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "Tagged with foo 1"
                , "Tagged with foo 2"
                , "Tagged with foo 3"
                , "Tagged with foo 4"
                , "Tagged with foo 5"
                , "Tagged with foo and bar 1"
                , "Tagged with foo and bar 2"
                , "Tagged with foo and bar 3"
                , "Tagged with foo and bar 4"
                , "Tagged with foo and bar 5"
                ]

      context "with a `tag` param" $
        it "responds with articles tagged with at least that tag" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              void $ createN 5 account $ \n ->
                createParams
                  { Attributes.title = "Tagged with foo " <> show n
                  , Attributes.tagList = Set.fromList ["foo"]
                  }
              void $ createN 5 account $ \n ->
                createParams
                  { Attributes.title = "Tagged with foo and bar " <> show n
                  , Attributes.tagList = Set.fromList ["foo", "bar"]
                  }
              lift $ get' "/api/articles/?tag=bar" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "Tagged with foo and bar 1"
                , "Tagged with foo and bar 2"
                , "Tagged with foo and bar 3"
                , "Tagged with foo and bar 4"
                , "Tagged with foo and bar 5"
                ]

      context "without an `author` param" $
        it "responds with articles by any author" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              author <-
                ExceptT $ register $ Registrant "secret123" "author@email.com" "author"
              void $ createN 5 account $ \n ->
                createParams
                  { Attributes.title = "By account " <> show n
                  }
              void $ createN 5 author $ \n ->
                createParams
                  { Attributes.title = "By author " <> show n
                  }
              lift $ get' "/api/articles/" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "By account 1"
                , "By account 2"
                , "By account 3"
                , "By account 4"
                , "By account 5"
                , "By author 1"
                , "By author 2"
                , "By author 3"
                , "By author 4"
                , "By author 5"
                ]

      context "with an `author` param" $
        it "responds with articles by that author" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              author <-
                ExceptT $ register $ Registrant "secret123" "author@email.com" "author"
              void $ createN 5 account $ \n ->
                createParams
                  { Attributes.title = "By account " <> show n
                  }
              void $ createN 5 author $ \n ->
                createParams
                  { Attributes.title = "By author " <> show n
                  }
              lift $ get' "/api/articles/?author=author" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "By author 1"
                , "By author 2"
                , "By author 3"
                , "By author 4"
                , "By author 5"
                ]

      context "without a `favorited` param" $
        it "responds with articles by any author" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              author <-
                ExceptT $ register $ Registrant "secret123" "author@email.com" "author"
              byAccount <- createN 5 account $ \n ->
                createParams
                  { Attributes.title = "Favorited by author " <> show n
                  }
              traverse_ (favorite author . Article.slug) byAccount
              void $ createN 5 author $ \n ->
                createParams
                  { Attributes.title = "Favorited by account " <> show n
                  }
              traverse_ (favorite account . Article.slug) byAccount
              lift $ get' "/api/articles/" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "Favorited by account 1"
                , "Favorited by account 2"
                , "Favorited by account 3"
                , "Favorited by account 4"
                , "Favorited by account 5"
                , "Favorited by author 1"
                , "Favorited by author 2"
                , "Favorited by author 3"
                , "Favorited by author 4"
                , "Favorited by author 5"
                ]

      context "with a `favorited` param" $
        it "responds with articles by that author" $ do
          res <-
            runExceptT $ do
              account <- defaultAccount
              author <-
                ExceptT $ register $ Registrant "secret123" "author@email.com" "author"
              byAccount <- createN 5 account $ \n ->
                createParams
                  { Attributes.title = "Favorited by author " <> show n
                  }
              traverse_ (favorite author . Article.slug) byAccount
              void $ createN 5 author $ \n ->
                createParams
                  { Attributes.title = "Favorited by account " <> show n
                  }
              traverse_ (favorite account . Article.slug) byAccount
              lift $ get' "/api/articles/?favorited=author" []
          liftIO $ do
            let articles = articlesFromResponse =<< res
            sort . (Article.title <$>) <$>
              articles `shouldBe`
              Right
                [ "Favorited by author 1"
                , "Favorited by author 2"
                , "Favorited by author 3"
                , "Favorited by author 4"
                , "Favorited by author 5"
                ]
