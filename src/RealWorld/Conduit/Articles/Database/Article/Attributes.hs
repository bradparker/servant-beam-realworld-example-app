module RealWorld.Conduit.Articles.Database.Article.Attributes
  ( Attributes(..)
  , forInsert
  , forUpdate
  ) where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Validation (Validation(Failure, Success), toEither)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Articles.Database.Article (Article)
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.Queries (findBySlug)
import RealWorld.Conduit.Attribute (Attribute)

data Attributes f = Attributes
  { slug :: Attribute f Text
  , title :: Attribute f Text
  , description :: Attribute f Text
  , body :: Attribute f Text
  }

type Errors = Map Text [Text]

generateSlug :: Text -> Text
generateSlug =
  Text.intercalate "-" .
  Text.words . Text.toLower . Text.filter ((||) <$> isAlphaNum <*> isSpace)

insertSlug ::
     Connection
  -> Text
  -> Compose IO (Validation Errors) Text
insertSlug conn title =
  Compose $ do
    existing <- findBySlug conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure (Map.singleton "title" ["Would produce duplicate slug: " <> value])
  where
    value = generateSlug title

require :: Text -> Text -> Validation Errors Text
require attr value =
  if Text.null value
    then Failure (Map.singleton attr ["Required"])
    else Success value

makeTitle :: Text -> Validation Errors Text
makeTitle = require "title"

makeDescription :: Text -> Validation Errors Text
makeDescription = require "description"

makeBody :: Text -> Validation Errors Text
makeBody = require "body"

forInsert ::
     Connection
  -> Text
  -> Text
  -> Text
  -> ExceptT Errors IO (Attributes Identity)
forInsert conn title description body =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> insertSlug conn title
    <*> Compose (pure (makeTitle title))
    <*> Compose (pure (makeDescription description))
    <*> Compose (pure (makeBody body))

makeUpdateSlug ::
     Connection
  -> Article
  -> Text
  -> Compose IO (Validation Errors) Text
makeUpdateSlug conn current title
  | value == Article.slug current = pure value
  | otherwise = insertSlug conn title
  where
    value = generateSlug title

forUpdate ::
     Connection
  -> Article
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ExceptT Errors IO (Attributes Maybe)
forUpdate conn current title description body =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> traverse (makeUpdateSlug conn current) title
    <*> traverse (Compose . pure . makeTitle) title
    <*> traverse (Compose . pure . makeDescription) description
    <*> traverse (Compose . pure . makeBody) body
