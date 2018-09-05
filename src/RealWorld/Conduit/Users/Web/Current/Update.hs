module RealWorld.Conduit.Users.Web.Current.Update
  ( Update
  , handler
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Pool (withResource)
import Data.String (String)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId))
import RealWorld.Conduit.Users.Database.User.Attributes (ValidationFailure)
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant
  ( Handler(Handler)
  , ServantErr
  , err401
  , err422
  , err500
  , errBody
  , throwError
  )
import Servant.API ((:>), JSON, Put, ReqBody)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import System.IO (IO)
import Text.Show (show)

type Update =
  Auth '[JWT] Claim :>
  "api" :>
  "user" :>
  ReqBody '[JSON] (Namespace "user" UserUpdate) :>
  Put '[ JSON] (Namespace "user" Account)

data Error
  = NotFound
  | FailedValidation [ValidationFailure]
  | InternalServerError String

data ErrorPayload e = ErrorPayload
  { message :: Text
  , errors :: e
  } deriving (Generic)

deriving instance ToJSON e => ToJSON (ErrorPayload e)

data UserUpdate = UserUpdate
  { password :: Maybe Text
  , email :: Maybe Text
  , username :: Maybe Text
  , bio :: Maybe Text
  , image :: Maybe (Maybe Text)
  }

deriving instance Generic UserUpdate
deriving instance FromJSON UserUpdate
deriving instance ToSchema UserUpdate

handler ::
     Handle
  -> AuthResult Claim
  -> Namespace "user" UserUpdate
  -> Handler (Namespace "user" Account)
handler handle (Authenticated claim) (Namespace params) =
  Handler $
  withExceptT toServantError $ Namespace <$> update handle claim params
handler _ _ _ =
  throwError err401 {errBody = encode (ErrorPayload "Unauthorized" Text.empty)}

toServantError :: Error -> ServantErr
toServantError NotFound =
  err401 {errBody = encode (ErrorPayload "Incorrect credentials" Text.empty)}
toServantError (FailedValidation e) =
  err422 {errBody = encode (ErrorPayload "Failed validation" e)}
toServantError (InternalServerError e) =
  err500 {errBody = encode (ErrorPayload "Internal server error" e)}

update :: Handle -> Claim -> UserUpdate -> ExceptT Error IO Account
update Handle {connectionPool, jwtSettings} (Claim id) params =
  withResource connectionPool $ \conn -> do
    user <-
      maybeToExceptT NotFound $ MaybeT $ Database.find conn (UserId id)
    attributes <-
      withExceptT FailedValidation $
      Attributes.forUpdate
        conn
        user
        (password params)
        (email params)
        (username params)
        (bio params)
        (image params)
    updated <- lift $ Database.update conn user attributes
    withExceptT (InternalServerError . show) $ fromUser jwtSettings updated
