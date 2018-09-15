module RealWorld.Conduit.Users.Web.Current.Update
  ( Update
  , handler
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Aeson (FromJSON)
import Data.Function (($), (.), flip)
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId))
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Auth (withRequiredAuth)
import RealWorld.Conduit.Web.Errors
  ( failedValidation
  , internalServerError
  , notFound
  )
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr)
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
handler handle authresult (Namespace params) =
  flip withRequiredAuth authresult $ \claim ->
    Handler $ Namespace <$> update handle claim params

update :: Handle -> Claim -> UserUpdate -> ExceptT ServantErr IO Account
update Handle {withDatabaseConnection, jwtSettings} (Claim id) params =
  withDatabaseConnection $ \conn -> do
    user <-
      maybeToExceptT (notFound "User") $ MaybeT $ Database.find conn (UserId id)
    attributes <-
      withExceptT failedValidation $
      Attributes.forUpdate
        conn
        user
        (password params)
        (email params)
        (username params)
        (bio params)
        (image params)
    updated <- lift $ Database.update conn user attributes
    withExceptT (internalServerError . Text.pack . show) $ fromUser jwtSettings updated
