module RealWorld.Conduit.Articles.DatabaseSpec
  ( spec
  ) where

import Database.Beam (primaryKey)
import qualified RealWorld.Conduit.Articles.Article as Article
import RealWorld.Conduit.Articles.Database (create, update)
import RealWorld.Conduit.Articles.Article.Attributes
  ( Attributes(Attributes)
  )
import qualified RealWorld.Conduit.Articles.Article.Attributes as Attributes
import RealWorld.Conduit.Spec.Database (withConnection)
import qualified RealWorld.Conduit.Users.Database as User
import qualified RealWorld.Conduit.Users.User.Attributes as UserAttributes
import Test.Hspec (Spec, around, describe, it, shouldBe)

userCreateParams :: UserAttributes.Attributes Identity
userCreateParams =
  UserAttributes.Attributes
    { UserAttributes.password = "password123"
    , UserAttributes.email = "user@example.com"
    , UserAttributes.username = "Username"
    , UserAttributes.bio = ""
    , UserAttributes.image = Nothing
    }

createParams :: Attributes Identity
createParams =
  Attributes
    { Attributes.title = "Title"
    , Attributes.description = "Description."
    , Attributes.body = "Body"
    , Attributes.tagList = mempty
    }

spec :: Spec
spec = around withConnection $ do
  describe "create"
    $ it "creates a Article with the supplied params"
    $ \conn -> do
        Right user <- runExceptT $ usingReaderT conn $ User.create
          userCreateParams
        Right article <- runExceptT $ usingReaderT conn $ create
          (primaryKey user)
          createParams
        Article.slug article `shouldBe` "title"
        Article.title article `shouldBe` "Title"
        Article.description article `shouldBe` "Description."
        Article.body article `shouldBe` "Body"

  describe "update"
    $ it "updates a article specified by articlename with new attributes"
    $ \conn -> do
        Right user <- runExceptT $ usingReaderT conn $ User.create
          userCreateParams
        Right article <- runExceptT $ usingReaderT conn $ create
          (primaryKey user)
          createParams
        Right updated <- runExceptT $ usingReaderT conn $ update
          (primaryKey user)
          (Article.slug article)
          Attributes
            { Attributes.title       = Nothing
            , Attributes.description = Nothing
            , Attributes.body        = Just "Now with a bigger body"
            , Attributes.tagList     = mempty
            }
        Article.title updated `shouldBe` Article.title article
        Article.description updated `shouldBe` Article.description article
        Article.body updated `shouldBe` "Now with a bigger body"
