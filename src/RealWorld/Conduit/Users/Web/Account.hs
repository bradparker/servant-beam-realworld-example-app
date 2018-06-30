module RealWorld.Conduit.Users.Web.Account
  ( Account(..)
  , fromUser
  ) where

import Control.Applicative ((<*>))
import Data.Aeson (ToJSON(..), Value(String), FromJSON)
import Data.Default (def)
import Data.Function ((.))
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import Text.Show (Show)
import Web.JWT (Algorithm(HS256), Secret, encodeSigned, unregisteredClaims)

data Account = Account
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Text
  , image :: Maybe Text
  } deriving (Show, Generic)

deriving instance ToJSON Account
deriving instance ToSchema Account
deriving instance FromJSON Account

deriveToken :: Secret -> Text -> Text
deriveToken key username =
  encodeSigned
    HS256
    key
    def {unregisteredClaims = Map.fromList [("username", String username)]}

fromUser :: Secret -> User -> Account
fromUser secret =
  Account
    <$> User.email
    <*> deriveToken secret . User.username
    <*> User.username
    <*> User.bio
    <*> User.image
