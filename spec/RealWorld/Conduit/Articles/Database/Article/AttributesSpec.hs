module RealWorld.Conduit.Articles.Database.Article.AttributesSpec
  ( spec
  ) where

import qualified Data.Map as Map
import Database.Beam (primaryKey)
import RealWorld.Conduit.Articles.Database
  ( attributesForInsert
  , attributesForUpdate
  , create
  )
import RealWorld.Conduit.Articles.Database.Article.Attributes
  ( Attributes(Attributes)
  )
import qualified RealWorld.Conduit.Articles.Database.Article.Attributes as Attributes
import RealWorld.Conduit.Spec.Database (withConnection)
import qualified RealWorld.Conduit.Users.Database as User
import qualified RealWorld.Conduit.Users.Database.User.Attributes as UserAttributes
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
    { Attributes.slug = "slug"
    , Attributes.title = "Title"
    , Attributes.description = "Description"
    , Attributes.body = "Body"
    , Attributes.tagList = mempty
    }

spec :: Spec
spec =
  around withConnection $ do
    describe "forInsert" $ do
      it "returns an Attributes Identity when valid" $ \conn -> do
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForInsert
            "Title"
            "Description"
            "Body"
            mempty
        Attributes.slug <$> attributes `shouldBe` Right "title"
        Attributes.title <$> attributes `shouldBe` Right "Title"
        Attributes.description <$> attributes `shouldBe` Right "Description"
        Attributes.body <$> attributes `shouldBe` Right "Body"

      it "returns a validation failure when title is taken" $ \conn -> do
        user <- User.create conn userCreateParams
        void $
          runExceptT $
          usingReaderT conn $
          create
            (primaryKey user)
            createParams {Attributes.title = "Taken", Attributes.slug = "taken"}
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForInsert
            "Taken"
            "Description"
            "Body"
            mempty
        Attributes.title <$> attributes `shouldBe` Left (Map.singleton "title" ["Would produce duplicate slug: taken"])

      it "returns a validation failure when description is absent" $ \conn -> do
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForInsert "Title" "" "Body" mempty
        Attributes.title <$> attributes `shouldBe` Left (Map.singleton "description" ["Required"])

      it "returns a validation failure when body is absent" $ \conn -> do
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForInsert "Title" "Description" "" mempty
        Attributes.title <$> attributes `shouldBe` Left (Map.singleton "body" ["Required"])

    describe "forUpdate" $ do
      it "returns an Attributes Maybe when valid" $ \conn -> do
        user <- liftIO $ User.create conn userCreateParams
        Right article <-
          runExceptT $
          usingReaderT conn $
          create (primaryKey user) createParams
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForUpdate
            article
            (Just "Title")
            (Just "A description")
            (Just "A body")
            Nothing
        Attributes.slug <$> attributes `shouldBe` Right (Just "title")
        Attributes.title <$> attributes `shouldBe` Right (Just "Title")
        Attributes.body <$> attributes `shouldBe` Right (Just "A body")

      it "returns a validation failure when title is taken" $ \conn -> do
        user <- User.create conn userCreateParams
        Right article <-
          runExceptT $
          usingReaderT conn $
          create (primaryKey user) createParams
        void $
          runExceptT $
          usingReaderT conn $
          create
            (primaryKey user)
            createParams
              {Attributes.title = "Taken", Attributes.slug = "taken"}
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForUpdate
            article
            (Just "Taken")
            Nothing
            Nothing
            Nothing
        Attributes.title <$> attributes `shouldBe` Left (Map.singleton "title" ["Would produce duplicate slug: taken"])

      it "returns a validation failure when description is absent" $ \conn -> do
        user <- User.create conn userCreateParams
        Right article <-
          runExceptT $
          usingReaderT conn $
          create (primaryKey user) createParams
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForUpdate article Nothing (Just "") Nothing Nothing
        Attributes.title <$> attributes `shouldBe` Left (Map.singleton "description" ["Required"])

      it "returns a validation failure when body is absent" $ \conn -> do
        user <- User.create conn userCreateParams
        Right article <-
          runExceptT $
          usingReaderT conn $
          create (primaryKey user) createParams
        attributes <-
          runExceptT $
          usingReaderT conn $
          attributesForUpdate article Nothing Nothing (Just "") Nothing
        Attributes.title <$> attributes `shouldBe` Left (Map.singleton "body" ["Required"])
