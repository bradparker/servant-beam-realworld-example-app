module RealWorld.Conduit.Users.Web.Account
  ( Account(..)
  , fromUser
  ) where

import Control.Applicative ((<*>), pure)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Aeson (FromJSON, ToJSON(..))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import qualified RealWorld.Conduit.Users.Web.Claim as Claim
import Servant.Auth.Server (JWTSettings)
import Text.Show (Show)
import Crypto.JOSE (Error)
import System.IO (IO)

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

fromUser :: JWTSettings -> User -> ExceptT Error IO Account
fromUser jwtSettings user =
  Account
    <$> pure (User.email user)
    <*> Claim.deriveToken jwtSettings user
    <*> pure (User.username user)
    <*> pure (User.bio user)
    <*> pure (User.image user)
