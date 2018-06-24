module RealWorld.Conduit.Users.Database.User.AttributesSpec
  ( spec
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Nothing, Just))
import RealWorld.Conduit.Spec.Database (withConnection)
import RealWorld.Conduit.Users.Database (create)
import RealWorld.Conduit.Users.Database.User.Attributes
  ( Attributes(Attributes)
  , ValidationFailure(EmailTaken, UsernameTaken)
  )
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
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
spec =
  around withConnection $ do
    describe "forInsert" $ do
      it "returns an Attributes Identity when valid" $ \conn -> do
        attributes <-
          runExceptT $
          Attributes.forInsert
            conn
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
        void $
          create
            conn
            createParams
              {Attributes.username = "Taken", Attributes.email = "tak@en.com"}
        attributes <-
          runExceptT $
          Attributes.forInsert
            conn
            "password123"
            "not-tak@en.com"
            "Taken"
            ""
            Nothing
        Attributes.username <$> attributes `shouldBe` Left [UsernameTaken]

      it "returns a validation failure when email is taken" $ \conn -> do
        void $
          create
            conn
            createParams
              {Attributes.username = "Taken", Attributes.email = "tak@en.com"}
        attributes <-
          runExceptT $
          Attributes.forInsert
            conn
            "password123"
            "tak@en.com"
            "Not Taken"
            ""
            Nothing
        Attributes.username <$> attributes `shouldBe` Left [EmailTaken]

    describe "forUpdate" $ do
      it "returns an Attributes Maybe when valid" $ \conn -> do
        user <- create conn createParams
        attributes <-
          runExceptT $
          Attributes.forUpdate
            conn
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
        user <- create conn createParams
        void $
          create
            conn
            createParams
              {Attributes.username = "Taken", Attributes.email = "tak@en.com"}
        attributes <-
          runExceptT $
          Attributes.forUpdate
            conn
            user
            (Just "password123")
            (Just "not-tak@en.com")
            (Just "Taken")
            Nothing
            Nothing
        Attributes.username <$> attributes `shouldBe` Left [UsernameTaken]

      it "returns a validation failure when email is taken" $ \conn -> do
        user <- create conn createParams
        void $
          create
            conn
            createParams
              {Attributes.username = "Taken", Attributes.email = "tak@en.com"}
        attributes <-
          runExceptT $
          Attributes.forUpdate
            conn
            user
            (Just "password123")
            (Just "tak@en.com")
            (Just "Not Taken")
            Nothing
            Nothing
        Attributes.username <$> attributes `shouldBe` Left [EmailTaken]
