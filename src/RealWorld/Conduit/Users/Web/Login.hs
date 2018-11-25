module RealWorld.Conduit.Users.Web.Login
  ( Login
  , handler
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson (ToJSON, encode)
import qualified Data.Text as Text
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr, err401, err500, errBody)
import Servant.API ((:>), JSON, Post, ReqBody)

type Login =
  "api" :>
  "users" :>
  "login" :>
  ReqBody '[JSON] (Namespace "user" Credentials) :>
  Post '[JSON] (Namespace "user" Account)

data Error
  = NotFound
  | InternalServerError Text

data ErrorPayload e = ErrorPayload
  { message :: Text
  , errors :: e
  } deriving (Generic)

deriving instance ToJSON e => ToJSON (ErrorPayload e)

handler ::
     Environment
  -> Namespace "user" Credentials
  -> Handler (Namespace "user" Account)
handler environment (Namespace params) =
  Handler $
  withExceptT toServantError $
  Namespace <$> login environment params

toServantError :: Error -> ServantErr
toServantError NotFound =
  err401 {errBody = encode (ErrorPayload "Incorrect credentials" Text.empty)}
toServantError (InternalServerError e) =
  err500 {errBody = encode (ErrorPayload "Internal server error" e)}

login :: Environment -> Credentials -> ExceptT Error IO Account
login Environment {withDatabaseConnection, jwtSettings} creds =
  withDatabaseConnection $ \conn -> do
    user <-
      maybeToExceptT NotFound $ MaybeT $ Database.findByCredentials conn creds
    withExceptT (InternalServerError . show) $ fromUser jwtSettings user
