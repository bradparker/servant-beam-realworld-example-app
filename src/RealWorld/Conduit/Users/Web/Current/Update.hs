module RealWorld.Conduit.Users.Web.Current.Update
  ( Update
  , handler
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson (FromJSON)
import Data.Swagger (ToSchema)
import RealWorld.Conduit.Environment (Environment(..))
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
     Environment
  -> AuthResult Claim
  -> Namespace "user" UserUpdate
  -> Handler (Namespace "user" Account)
handler environment authresult (Namespace params) = do
  user <- loadAuthorizedUser environment authresult
  updated <- Handler $ update environment user params
  Namespace <$> account environment updated

update :: Environment -> User -> UserUpdate -> ExceptT ServantErr IO User
update environment user params =
  withDatabaseConnection environment $ \conn -> do
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
