module RealWorld.Conduit.Users.Web.Login
  ( Login
  , handler
  ) where

import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Aeson (ToJSON, encode)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr, err401, err500, errBody)
import Servant.API ((:>), JSON, Post, ReqBody)
import System.IO (IO)
import Text.Show (show)

type Login =
  "api" :>
  "users" :>
  "login" :>
  ReqBody '[JSON] (Namespace "user" Credentials) :>
  Post '[JSON] (Namespace "user" Account)

data Error
  = NotFound
  | InternalServerError String

data ErrorPayload e = ErrorPayload
  { message :: Text
  , errors :: e
  } deriving (Generic)

deriving instance ToJSON e => ToJSON (ErrorPayload e)

handler ::
     Handle
  -> Namespace "user" Credentials
  -> Handler (Namespace "user" Account)
handler handle (Namespace params) =
  Handler $
  withExceptT toServantError $
  Namespace <$> login handle params

toServantError :: Error -> ServantErr
toServantError NotFound =
  err401 {errBody = encode (ErrorPayload "Incorrect credentials" Text.empty)}
toServantError (InternalServerError e) =
  err500 {errBody = encode (ErrorPayload "Internal server error" e)}

login :: Handle -> Credentials -> ExceptT Error IO Account
login Handle {withDatabaseConnection, jwtSettings} creds =
  withDatabaseConnection $ \conn -> do
    user <-
      maybeToExceptT NotFound $ MaybeT $ Database.findByCredentials conn creds
    withExceptT (InternalServerError . show) $ fromUser jwtSettings user
