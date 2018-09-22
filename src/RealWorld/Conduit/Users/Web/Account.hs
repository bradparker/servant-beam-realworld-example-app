module RealWorld.Conduit.Users.Web.Account
  ( Account(..)
  , fromUser
  , account
  ) where

import Control.Applicative ((<*>), pure)
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)
import Crypto.JOSE (Error)
import Data.Aeson (FromJSON, ToJSON(..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import qualified RealWorld.Conduit.Users.Web.Claim as Claim
import RealWorld.Conduit.Web.Errors (internalServerError)
import Servant (Handler(Handler))
import Servant.Auth.Server (JWTSettings)
import System.IO (IO)
import Text.Show (Show, show)

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

account :: Handle -> User -> Handler Account
account handle user =
  Handler $
  withExceptT (internalServerError . Text.pack . show) $
  fromUser (jwtSettings handle) user
