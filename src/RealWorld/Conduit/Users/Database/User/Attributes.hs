module RealWorld.Conduit.Users.Database.User.Attributes
  ( Attributes(..)
  , forInsert
  , forUpdate
  ) where

import Crypto.Scrypt
  ( EncryptedPass(getEncryptedPass)
  , Pass(Pass)
  , encryptPassIO'
  )
import Data.Aeson (ToJSON)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Validation (Validation(Failure, Success), toEither)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Attribute (Attribute)
import RealWorld.Conduit.Users.Database.Queries (findByEmail, findByUsername)
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User

data Attributes f = Attributes
  { password :: Attribute f Text
  , email :: Attribute f Text
  , username :: Attribute f Text
  , bio :: Attribute f Text
  , image :: Attribute f (Maybe Text)
  }

type Errors = Map Text [Text]

data ValidationFailure
  = EmailTaken
  | UsernameTaken
  | PasswordLessThan8Chars

deriving instance Generic ValidationFailure
deriving instance Show ValidationFailure
deriving instance Eq ValidationFailure
deriving instance ToJSON ValidationFailure

encryptPassword :: Text -> IO Text
encryptPassword password =
  decodeUtf8 . getEncryptedPass <$> encryptPassIO' (Pass (encodeUtf8 password))

makePassword :: Text -> Compose IO (Validation Errors) Text
makePassword value
  | Text.length value < 8 = Compose $ pure $ Failure (Map.singleton "password" ["Must be longer than 8 chars"])
  | otherwise = Compose $ Success <$> encryptPassword value

insertEmail ::
     Connection
  -> Text
  -> Compose IO (Validation Errors) Text
insertEmail conn value =
  Compose $ do
    existing <- findByEmail conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure (Map.singleton "email" ["Taken"])

insertUsername ::
     Connection
  -> Text
  -> Compose IO (Validation Errors) Text
insertUsername conn value =
  Compose $ do
    existing <- findByUsername conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure (Map.singleton "username" ["Taken"])

forInsert ::
     Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> ExceptT Errors IO (Attributes Identity)
forInsert conn passwordVal emailVal usernameVal bioVal imageVal =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> makePassword passwordVal
    <*> insertEmail conn emailVal
    <*> insertUsername conn usernameVal
    <*> pure bioVal
    <*> pure imageVal

makeUpdateEmail ::
     Connection
  -> User
  -> Text
  -> Compose IO (Validation Errors) Text
makeUpdateEmail conn current value
  | value == User.email current = pure value
  | otherwise = insertEmail conn value

makeUpdateUsername ::
     Connection
  -> User
  -> Text
  -> Compose IO (Validation Errors) Text
makeUpdateUsername conn current value
  | value == User.username current = pure value
  | otherwise = insertUsername conn value

forUpdate ::
     Connection
  -> User
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Maybe Text)
  -> ExceptT Errors IO (Attributes Maybe)
forUpdate conn current passwordVal emailVal usernameVal bioVal imageVal =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> traverse makePassword passwordVal
    <*> traverse (makeUpdateEmail conn current) emailVal
    <*> traverse (makeUpdateUsername conn current) usernameVal
    <*> pure bioVal
    <*> pure imageVal
