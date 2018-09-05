module RealWorld.Conduit.Users.Database.User.Attributes
  ( Attributes(..)
  , ValidationFailure(..)
  , forInsert
  , forUpdate
  ) where

import Control.Applicative ((<*>), pure)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Crypto.Scrypt
  ( EncryptedPass(getEncryptedPass)
  , Pass(Pass)
  , encryptPassIO'
  )
import Data.Aeson (ToJSON)
import Data.Bool (otherwise)
import Data.Eq (Eq, (==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord ((<))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Traversable (traverse)
import Data.Validation (Validation(Failure, Success), toEither)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import RealWorld.Conduit.Attribute (Attribute)
import RealWorld.Conduit.Users.Database.Queries (findByEmail, findByUsername)
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import System.IO (IO)
import Text.Show (Show)

data Attributes f = Attributes
  { password :: Attribute f Text
  , email :: Attribute f Text
  , username :: Attribute f Text
  , bio :: Attribute f Text
  , image :: Attribute f (Maybe Text)
  }

data ValidationFailure
  = EmailTaken
  | UsernameTaken
  | PasswordLessThan8Chars

deriving instance Generic ValidationFailure
deriving instance Show ValidationFailure
deriving instance Eq ValidationFailure
deriving instance ToJSON ValidationFailure

encryptPassword :: Text -> IO Text
encryptPassword pass =
  decodeUtf8 . getEncryptedPass <$> encryptPassIO' (Pass (encodeUtf8 pass))

makePassword :: Text -> Compose IO (Validation [ValidationFailure]) Text
makePassword value
  | Text.length value < 8 = Compose $ pure $ Failure [PasswordLessThan8Chars]
  | otherwise = Compose $ Success <$> encryptPassword value

insertEmail ::
     Connection
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
insertEmail conn value =
  Compose $ do
    existing <- findByEmail conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure [EmailTaken]

insertUsername ::
     Connection
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
insertUsername conn value =
  Compose $ do
    existing <- findByUsername conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure [UsernameTaken]

forInsert ::
     Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> ExceptT [ValidationFailure] IO (Attributes Identity)
forInsert conn passwordVal emailVal usernameVal bioVal imageVal =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> makePassword passwordVal
    <*> insertEmail conn emailVal
    <*> insertUsername conn usernameVal
    <*> pure bioVal
    <*> pure imageVal

updateEmail ::
     Connection
  -> User
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
updateEmail conn current value
  | value == User.email current = pure value
  | otherwise = insertEmail conn value

updateUsername ::
     Connection
  -> User
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
updateUsername conn current value
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
  -> ExceptT [ValidationFailure] IO (Attributes Maybe)
forUpdate conn current passwordVal emailVal usernameVal bioVal imageVal =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> traverse makePassword passwordVal
    <*> traverse (updateEmail conn current) emailVal
    <*> traverse (updateUsername conn current) usernameVal
    <*> pure bioVal
    <*> pure imageVal
