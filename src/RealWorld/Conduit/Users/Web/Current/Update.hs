module RealWorld.Conduit.Users.Web.Current.Update
  ( Update
  , handler
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Data.Aeson (FromJSON)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (User)
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
import RealWorld.Conduit.Users.Web.Account (Account, account)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (failedValidation)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr)
import Servant.API ((:>), JSON, Put, ReqBody)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import System.IO (IO)

type Update =
  "api" :>
  "user" :>
  Auth '[JWT] Claim :>
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
handler handle authresult (Namespace params) = do
  user <- loadAuthorizedUser handle authresult
  updated <- Handler $ update handle user params
  Namespace <$> account handle updated

update :: Handle -> User -> UserUpdate -> ExceptT ServantErr IO User
update handle user params =
  withDatabaseConnection handle $ \conn -> do
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
    lift $ Database.update conn user attributes
