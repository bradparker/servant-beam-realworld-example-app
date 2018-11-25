module RealWorld.Conduit.Users.Web.Account
  ( Account(..)
  , fromUser
  , account
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Crypto.JOSE (Error)
import Data.Aeson (FromJSON, ToJSON(..))
import Data.Swagger (ToSchema)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User as User
import qualified RealWorld.Conduit.Users.Web.Claim as Claim
import RealWorld.Conduit.Web.Errors (internalServerError)
import Servant (Handler(Handler))
import Servant.Auth.Server (JWTSettings)

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

account :: Environment -> User -> Handler Account
account environment user =
  Handler $
  withExceptT (internalServerError . show) $
  fromUser (jwtSettings environment) user
