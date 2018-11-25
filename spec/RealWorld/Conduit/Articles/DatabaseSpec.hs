module RealWorld.Conduit.Articles.DatabaseSpec
  ( spec
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Database
  ( create
  , findBySlug
  , findByTitle
  , update
  )
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.Article.Attributes
  ( Attributes(Attributes)
  )
import qualified RealWorld.Conduit.Articles.Database.Article.Attributes as Attributes
import RealWorld.Conduit.Spec.Database (withConnection)
import qualified RealWorld.Conduit.Users.Database as User
import qualified RealWorld.Conduit.Users.Database.User.Attributes as UserAttributes
import Test.Hspec (Spec, around, context, describe, it, shouldBe)

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
    { Attributes.slug = "slug"
    , Attributes.title = "Title"
    , Attributes.description = "Description."
    , Attributes.body = "Body"
    }

spec :: Spec
spec =
  around withConnection $ do
    describe "create" $
      it "creates a Article with the supplied params" $ \conn -> do
        user <- User.create conn userCreateParams
        article <- create conn (primaryKey user) createParams
        Article.slug article `shouldBe` "slug"
        Article.title article `shouldBe` "Title"
        Article.description article `shouldBe` "Description."
        Article.body article `shouldBe` "Body"

    describe "update" $
      it "updates a article specified by articlename with new attributes" $ \conn -> do
        user <- User.create conn userCreateParams
        article <- create conn (primaryKey user) createParams
        updated <-
          update
            conn
            article
            Attributes
              { Attributes.slug = Nothing
              , Attributes.title = Nothing
              , Attributes.description = Nothing
              , Attributes.body = Just "Now with a bigger body"
              }
        primaryKey article `shouldBe` primaryKey updated
        Article.title updated `shouldBe` Article.title article
        Article.description updated `shouldBe` Article.description article
        Article.body updated `shouldBe` "Now with a bigger body"

    describe "findBySlug" $
      context "when the article exists" $
        it "returns (Just matching)" $ \conn -> do
          user <- User.create conn userCreateParams
          article <- create conn (primaryKey user) createParams
          found <- findBySlug conn "slug"
          primaryKey <$> found `shouldBe` Just (primaryKey article)
          Article.title <$> found `shouldBe` Just "Title"

    describe "findByTitle" $
      context "when the article exists" $
        it "returns (Just matching)" $ \conn -> do
          user <- User.create conn userCreateParams
          article <- create conn (primaryKey user) createParams
          found <- findByTitle conn "Title"
          primaryKey <$> found `shouldBe` Just (primaryKey article)
          Article.slug <$> found `shouldBe` Just "slug"
