module RealWorld.Conduit.Users.DatabaseSpec
  ( spec
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Spec.Database (withConnection)
import RealWorld.Conduit.Users.Database
  ( create
  , findByEmail
  , findByUsername
  , update
  )
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User.Attributes (Attributes(Attributes))
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
import Test.Hspec (Spec, around, context, describe, it, shouldBe)

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
    describe "create" $
      it "creates a User with the supplied params" $ \conn -> do
        user <- create conn createParams
        User.email user `shouldBe` "user@example.com"
        User.username user `shouldBe` "Username"
        User.bio user `shouldBe` ""
        User.image user `shouldBe` Nothing

    describe "update" $
      it "updates a user specified by username with new attributes" $ \conn -> do
        user <- create conn createParams
        updated <-
          update
            conn
            user
            Attributes
              { Attributes.password = Nothing
              , Attributes.email = Nothing
              , Attributes.username = Nothing
              , Attributes.bio = Just "Now with a bio"
              , Attributes.image = Just (Just "http://an.image/woot.jpg")
              }
        primaryKey user `shouldBe` primaryKey updated
        User.username updated `shouldBe` User.username user
        User.email updated `shouldBe` User.email user
        User.bio updated `shouldBe` "Now with a bio"
        User.image updated `shouldBe` Just "http://an.image/woot.jpg"

    describe "findByEmail" $
      context "when the user exists" $
        it "returns (Just matching)" $ \conn -> do
          user <- create conn createParams
          found <- findByEmail conn "user@example.com"
          primaryKey <$> found `shouldBe` Just (primaryKey user)
          User.username <$> found `shouldBe` Just "Username"

    describe "findByUsername" $
      context "when the user exists" $
        it "returns (Just matching)" $ \conn -> do
          user <- create conn createParams
          found <- findByUsername conn "Username"
          primaryKey <$> found `shouldBe` Just (primaryKey user)
          User.email <$> found `shouldBe` Just "user@example.com"
