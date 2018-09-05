module RealWorld.Conduit.Users.Web.Current.View
  ( View
  , handler
  ) where

import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Aeson (ToJSON, encode)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Pool (withResource)
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId))
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant
  ( Handler(Handler)
  , ServantErr
  , err401
  , err500
  , errBody
  , throwError
  )
import Servant.API ((:>), JSON, Get)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import System.IO (IO)
import Text.Show (show)

type View =
  Auth '[JWT] Claim :>
  "api" :>
  "user" :>
  Get '[JSON] (Namespace "user" Account)

data Error
  = NotFound
  | InternalServerError String

data ErrorPayload e = ErrorPayload
  { message :: Text
  , errors :: e
  } deriving (Generic)

deriving instance ToJSON e => ToJSON (ErrorPayload e)

handler :: Handle -> AuthResult Claim -> Handler (Namespace "user" Account)
handler handle (Authenticated claim) =
  Handler $ withExceptT toServantError $ Namespace <$> view handle claim
handler _ _ =
  throwError err401 {errBody = encode (ErrorPayload "Unauthorized" Text.empty)}

toServantError :: Error -> ServantErr
toServantError NotFound =
  err401 {errBody = encode (ErrorPayload "Incorrect credentials" Text.empty)}
toServantError (InternalServerError e) =
  err500 {errBody = encode (ErrorPayload "Internal server error" e)}

view :: Handle -> Claim -> ExceptT Error IO Account
view Handle {connectionPool, jwtSettings} (Claim id) =
  withResource connectionPool $ \conn -> do
    user <-
      maybeToExceptT NotFound $ MaybeT $ Database.find conn (UserId id)
    withExceptT (InternalServerError . show) $ fromUser jwtSettings user
