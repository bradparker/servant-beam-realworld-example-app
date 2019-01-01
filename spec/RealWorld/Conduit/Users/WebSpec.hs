module RealWorld.Conduit.Users.WebSpec
  ( spec
  , register
  ) where

import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types
  ( hAuthorization
  , status200
  , status201
  , status400
  , status401
  , status404
  , status422
  )
import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleBody, simpleStatus))
import Prelude hiding (ByteString)
import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Spec.Web (withApp)
import RealWorld.Conduit.Users.Web (server, users)
import qualified RealWorld.Conduit.Users.Web.Account as Account
import RealWorld.Conduit.Users.Web.Account (Account)
import RealWorld.Conduit.Users.Profile (Profile)
import qualified RealWorld.Conduit.Users.Profile as Profile
import RealWorld.Conduit.Users.Web.Register (Registrant(Registrant))
import qualified RealWorld.Conduit.Web as Web
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace), unNamespace)
import Servant (serveWithContext)
import Test.Hspec (Spec, around, context, describe, it, shouldBe)
import Test.Hspec.Wai.Extended (WaiSession, delete', get', post', put')
import Test.Hspec.Wai.JSON (json)

app :: Environment -> Application
app handle = serveWithContext users (Web.context handle) (server handle)

decodeUserNamespace :: FromJSON a => ByteString -> Either String (Namespace "user" a)
decodeUserNamespace = eitherDecode

userFromResponse :: FromJSON a => SResponse -> Either String a
userFromResponse = (unNamespace <$>) . decodeUserNamespace . simpleBody

accountFromResponse :: SResponse -> Either String Account
accountFromResponse = userFromResponse

decodeProfileNamespace :: FromJSON a => ByteString -> Either String (Namespace "profile" a)
decodeProfileNamespace = eitherDecode

profileFromResponse :: SResponse -> Either String Profile
profileFromResponse = (unNamespace <$>) . decodeProfileNamespace . simpleBody

userNamespace :: a -> Namespace "user" a
userNamespace = Namespace

register :: Registrant -> WaiSession (Either String Account)
register =
  (accountFromResponse <$>) . post' "/api/users" [] . encode . userNamespace

spec :: Spec
spec =
  around (withApp app) $ do
    describe "POST /api/users" $ do
      context "when provided a valid body with valid values" $
        it "responds with 201 and a json encoded user response" $ do
          res <-
            post'
              "/api/users"
              []
              [json|{
                user: {
                  username: "aname",
                  email: "e@mail.com",
                  password: "secret123"
                }
              }|]
          liftIO $ do
            simpleStatus res `shouldBe` status201
            Account.username <$> accountFromResponse res `shouldBe` Right "aname"
            Account.email <$> accountFromResponse res `shouldBe` Right "e@mail.com"

      context "when provided a valid body with invalid values" $
        it "responds with 422 and a json encoded error response" $ do
          void $ register $ Registrant "secret123" "also@taken.com" "taken"
          res <-
            post'
              "/api/users"
              []
              [json|{
                user: {
                  username: "taken",
                  email: "also@taken.com",
                  password: "secret123"
                }
              }|]
          liftIO $ do
            simpleStatus res `shouldBe` status422
            simpleBody res `shouldBe`
              [json|{
                message: "Failed validation",
                errors: {
                  email: ["Taken"],
                  username: ["Taken"]
                }
              }|]

      context "when provided an invalid body" $
        it "responds with 400" $ do
          res <-
            post'
              "/api/users"
              []
              [json|{
                user: {
                  wrong: "params"
                }
              }|]
          liftIO $ do
            simpleStatus res `shouldBe` status400
            simpleBody res `shouldBe` "Error in $.user: key \"password\" not present"

    describe "POST /api/users/login" $ do
      context "when provided the correct credentials" $
        it "responds with 200 and a json encoded user response" $ do
          void $ register $ Registrant "secret123" "e@mail.com" "aname"
          res <-
            post'
              "/api/users/login"
              []
              [json|{
                user: {
                  email: "e@mail.com",
                  password: "secret123"
                }
              }|]
          liftIO $ do
            simpleStatus res `shouldBe` status200
            Account.username <$> accountFromResponse res `shouldBe` Right "aname"
            Account.email <$> accountFromResponse res `shouldBe` Right "e@mail.com"

      context "when provided incorrect credentials" $
        it "responds with a 401" $ do
          void $ register $ Registrant "secret123" "e@mail.com" "username"
          res <-
            post'
              "/api/users/login"
              []
              [json|{
                user: {
                  email: "e@mail.com",
                  password: "wrong just wrong"
                }
              }|]
          liftIO $ simpleStatus res `shouldBe` status401

    describe "GET /api/user" $ do
      context "when provided a valid token" $
        it "responds with 200 and a json encoded user response" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                get'
                  "/api/user"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
          liftIO $ do
            simpleStatus <$> res `shouldBe` Right status200
            Account.username <$>
              (accountFromResponse =<< res) `shouldBe` Right "aname"
            Account.email <$>
              (accountFromResponse =<< res) `shouldBe` Right "e@mail.com"

      context "when provided an invalid token" $
        it "responds with a 401" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                get'
                  "/api/user"
                  [ ( hAuthorization
                    , encodeUtf8 ("Bearer " <> Account.token account <> "wrong"))
                  ]
          liftIO $ simpleStatus <$> res `shouldBe` Right status401

    describe "PUT /api/user" $ do
      context "when provided an invalid token" $
        it "responds with a 401" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                put'
                  "/api/user"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account <> "wrong"))]
                  [json|{
                    user: {
                      email: "changed@mail.com"
                    }
                  }|]
          liftIO $ simpleStatus <$> res `shouldBe` Right status401

      context "when provided a valid body of attributes to update" $
        it "responds with 200 and a json encoded response of the updated user" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                put'
                  "/api/user"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                    user: {
                      email: "changed@mail.com"
                    }
                  }|]
          liftIO $ do
            simpleStatus <$> res `shouldBe` Right status200
            Account.email <$>
              (accountFromResponse =<< res) `shouldBe` Right "changed@mail.com"

      context "when provided an invalid body of attributes to update" $
        it "responds with 422 and a json encoded error response" $ do
          void $ register $ Registrant "secret123" "ta@ken.com" "taken"
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                put'
                  "/api/user"
                  [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                  [json|{
                    user: {
                      email: "ta@ken.com"
                    }
                  }|]
          liftIO $ do
            simpleStatus <$> res `shouldBe` Right status422
            simpleBody <$>
              res `shouldBe`
              Right
                [json|{
                  message: "Failed validation",
                  errors: {
                    email: ["Taken"]
                  }
                }|]

    describe "GET /api/profiles/:username" $ do
      context "when user exists" $
        it "returns 200 with the user as a json encoded Profile" $ do
          void $ register $ Registrant "secret123" "e@mail.com" "usernameA"
          res <- get' "/api/profiles/usernameA" []
          liftIO $ do
            simpleStatus res `shouldBe` status200
            let profile = profileFromResponse res
            Profile.username <$> profile `shouldBe` Right "usernameA"
            Profile.bio <$> profile `shouldBe` Right ""
            Profile.image <$> profile `shouldBe` Right Nothing

      context "when user doesn't exist" $
        it "returns 404 with a message" $ do
          void $ register $ Registrant "secret123" "e@mail.com" "usernameA"
          res <- get' "/api/profiles/usernameB" []
          liftIO $ do
            simpleStatus res `shouldBe` status404
            simpleBody res `shouldBe`
              [json|{ message: "Profile not found", errors: null }|]

    describe "POST /api/profiles/:username/follow" $ do
      context "when user isn't authenticated" $
        it "responds with a 401" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                post'
                  "/api/profiles/followee/follow"
                  [ ( hAuthorization
                    , encodeUtf8 ("Bearer " <> Account.token account <> "wrong"))
                  ]
                  ""
          liftIO $ simpleStatus <$> res `shouldBe` Right status401

      context "when user is authenticated" $ do
        context "when profile exists" $
          it "returns 200 with the profile as json" $ do
            res <-
              runExceptT $ do
                void $ ExceptT $ register $ Registrant "secret123" "follow@ee.com" "followee"
                account <-
                  ExceptT $ register $ Registrant "secret123" "follow@er.com" "follower"
                lift $
                  post'
                    "/api/profiles/followee/follow"
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                    ""
            liftIO $ do
              simpleStatus <$> res `shouldBe` Right status200
              let profile = profileFromResponse =<< res
              Profile.username <$> profile `shouldBe` Right "followee"
              Profile.bio <$> profile `shouldBe` Right ""
              Profile.image <$> profile `shouldBe` Right Nothing

        context "when profile does not exist" $
          it "responds with a 404" $ do
            res <-
              runExceptT $ do
                account <-
                  ExceptT $ register $ Registrant "secret123" "follow@er.com" "follower"
                lift $
                  post'
                    "/api/profiles/followee/follow"
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
                    ""
            liftIO $
              simpleStatus <$> res `shouldBe` Right status404

    describe "DELETE /api/profiles/:username/follow" $ do
      context "when user isn't authenticated" $
        it "responds with a 401" $ do
          res <-
            runExceptT $ do
              account <-
                ExceptT $ register $ Registrant "secret123" "e@mail.com" "aname"
              lift $
                delete'
                  "/api/profiles/followee/follow"
                  [ ( hAuthorization
                    , encodeUtf8 ("Bearer " <> Account.token account <> "wrong"))
                  ]
          liftIO $ simpleStatus <$> res `shouldBe` Right status401

      context "when user is authenticated" $ do
        context "when profile exists" $
          it "returns 204 with the profile as json" $ do
            res <-
              runExceptT $ do
                void $ ExceptT $ register $ Registrant "secret123" "follow@ee.com" "followee"
                account <-
                  ExceptT $ register $ Registrant "secret123" "follow@er.com" "follower"
                lift $
                  delete'
                    "/api/profiles/followee/follow"
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
            liftIO $ do
              simpleStatus <$> res `shouldBe` Right status200
              let profile = profileFromResponse =<< res
              Profile.username <$> profile `shouldBe` Right "followee"
              Profile.bio <$> profile `shouldBe` Right ""
              Profile.image <$> profile `shouldBe` Right Nothing

        context "when profile does not exist" $
          it "responds with a 404" $ do
            res <-
              runExceptT $ do
                account <-
                  ExceptT $ register $ Registrant "secret123" "follow@er.com" "follower"
                lift $
                  delete'
                    "/api/profiles/followee/follow"
                    [(hAuthorization, encodeUtf8 ("Bearer " <> Account.token account))]
            liftIO $
              simpleStatus <$> res `shouldBe` Right status404
