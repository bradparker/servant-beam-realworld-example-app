module RealWorld.Conduit.Users.DatabaseSpec
  ( spec
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Spec.Database (withConnection)
import RealWorld.Conduit.Users.Database
  ( create
  , update
  )
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User (PrimaryKey(unUserId))
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
  describe "create" $ it "creates a User with the supplied params" $ \conn -> do
    Right user <- runExceptT $ usingReaderT conn $ create createParams
    User.email user `shouldBe` "user@example.com"
    User.username user `shouldBe` "Username"
    User.bio user `shouldBe` ""
    User.image user `shouldBe` Nothing

  describe "update"
    $ it "updates a user specified by username with new attributes"
    $ \conn -> do
        Right user    <- runExceptT $ usingReaderT conn $ create createParams
        Right updated <- runExceptT $ usingReaderT conn $ update
          (unUserId . primaryKey $ user)
          Attributes
            { Attributes.password = Nothing
            , Attributes.email    = Nothing
            , Attributes.username = Nothing
            , Attributes.bio      = Just "Now with a bio"
            , Attributes.image    = Just (Just "http://an.image/woot.jpg")
            }
        primaryKey user `shouldBe` primaryKey updated
        User.username updated `shouldBe` User.username user
        User.email updated `shouldBe` User.email user
        User.bio updated `shouldBe` "Now with a bio"
        User.image updated `shouldBe` Just "http://an.image/woot.jpg"
