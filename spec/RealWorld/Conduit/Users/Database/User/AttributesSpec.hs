module RealWorld.Conduit.Users.Database.User.AttributesSpec
  ( spec
  ) where

import qualified Data.Map as Map
import RealWorld.Conduit.Spec.Database (withConnection)
import RealWorld.Conduit.Users.Database (create, attributesForInsert, attributesForUpdate)
import RealWorld.Conduit.Users.User.Attributes (Attributes(Attributes))
import qualified RealWorld.Conduit.Users.User.Attributes as Attributes
import Test.Hspec (Spec, around, describe, it, shouldBe)

createParams :: Attributes Identity
createParams =
  Attributes
    { Attributes.password = "password123"
    , Attributes.email = "user@example.com"
    , Attributes.username = "Username"
    , Attributes.bio = ""
    , Attributes.image = Nothing
    }

spec :: Spec
spec = around withConnection $ do
  describe "forInsert" $ do
    it "returns an Attributes Identity when valid" $ \conn -> do
      attributes <- runExceptT $ usingReaderT conn $ attributesForInsert
        "password123"
        "user@example.com"
        "Username"
        ""
        Nothing
      Attributes.email <$> attributes `shouldBe` Right "user@example.com"
      Attributes.username <$> attributes `shouldBe` Right "Username"
      Attributes.bio <$> attributes `shouldBe` Right ""
      Attributes.image <$> attributes `shouldBe` Right Nothing

    it "returns a validation failure when username is taken" $ \conn -> do
      void $ runExceptT $ usingReaderT conn $ create createParams
        { Attributes.username = "Taken"
        , Attributes.email    = "tak@en.com"
        }
      attributes <- runExceptT $ usingReaderT conn $ attributesForInsert
        "password123"
        "not-tak@en.com"
        "Taken"
        ""
        Nothing
      Attributes.username <$> attributes `shouldBe` Left
        (Map.singleton "username" ["Taken"])

    it "returns a validation failure when email is taken" $ \conn -> do
      void $ runExceptT $ usingReaderT conn $ create createParams
        { Attributes.username = "Taken"
        , Attributes.email    = "tak@en.com"
        }
      attributes <- runExceptT $ usingReaderT conn $ attributesForInsert
        "password123"
        "tak@en.com"
        "Not Taken"
        ""
        Nothing
      Attributes.username <$> attributes `shouldBe` Left
        (Map.singleton "email" ["Taken"])

  describe "forUpdate" $ do
    it "returns an Attributes Maybe when valid" $ \conn -> do
      Right user <- runExceptT $ usingReaderT conn $ create createParams
      attributes <- runExceptT $ usingReaderT conn $ attributesForUpdate
        user
        (Just "password123")
        (Just "user@example.com")
        (Just "Username")
        (Just "A bio")
        Nothing
      Attributes.email <$> attributes `shouldBe` Right (Just "user@example.com")
      Attributes.username <$> attributes `shouldBe` Right (Just "Username")
      Attributes.bio <$> attributes `shouldBe` Right (Just "A bio")
      Attributes.image <$> attributes `shouldBe` Right Nothing

    it "returns a validation failure when username is taken" $ \conn -> do
      Right user <- runExceptT $ usingReaderT conn $ create createParams
      void $ runExceptT $ usingReaderT conn $ create createParams
        { Attributes.username = "Taken"
        , Attributes.email    = "tak@en.com"
        }
      attributes <- runExceptT $ usingReaderT conn $ attributesForUpdate
        user
        (Just "password123")
        (Just "not-tak@en.com")
        (Just "Taken")
        Nothing
        Nothing
      Attributes.username <$> attributes `shouldBe` Left
        (Map.singleton "username" ["Taken"])

    it "returns a validation failure when email is taken" $ \conn -> do
      Right user <- runExceptT $ usingReaderT conn $ create createParams
      void $ runExceptT $ usingReaderT conn $ create createParams
        { Attributes.username = "Taken"
        , Attributes.email    = "tak@en.com"
        }
      attributes <- runExceptT $ usingReaderT conn $ attributesForUpdate
        user
        (Just "password123")
        (Just "tak@en.com")
        (Just "Not Taken")
        Nothing
        Nothing
      Attributes.username <$> attributes `shouldBe` Left
        (Map.singleton "email" ["Taken"])
