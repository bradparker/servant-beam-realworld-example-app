module RealWorld.Conduit.Users.Database.User.AttributesSpec
  ( spec
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Nothing))
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
  around withConnection $
    describe "createAttributes" $ do
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
            "tak@en.com"
            "Not Taken"
            ""
            Nothing
        Attributes.username <$> attributes `shouldBe` Left [EmailTaken]
