module RealWorld.Conduit.Users.Web.Current.Update
  ( Update
  , handler
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson (FromJSON(parseJSON), (.:!), withObject)
import Data.Swagger (ToSchema)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (User, PrimaryKey(unUserId))
import RealWorld.Conduit.Users.Web.Account (Account, account)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (failedValidation, internalServerError)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler))
import Servant.API ((:>), JSON, Put, ReqBody)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import Database.Beam (primaryKey)

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
deriving instance ToSchema UserUpdate

instance FromJSON UserUpdate where
  parseJSON = withObject "UserUpdate" $ \v ->
    UserUpdate
      <$> v .:! "password"
      <*> v .:! "email"
      <*> v .:! "username"
      <*> v .:! "bio"
      <*> v .:! "image"

handler ::
     Environment
  -> AuthResult Claim
  -> Namespace "user" UserUpdate
  -> Handler (Namespace "user" Account)
handler environment authresult (Namespace params) = do
  user <- loadAuthorizedUser environment authresult
  updated <- update environment user params
  Namespace <$> account environment updated

update :: Environment -> User -> UserUpdate -> Handler User
update environment user params =
  Handler $
  withDatabaseConnection environment $ \conn -> do
    attributes <-
      withExceptT failedValidation $
      usingReaderT conn $
      Database.attributesForUpdate
        user
        (password params)
        (email params)
        (username params)
        (bio params)
        (image params)
    withExceptT (internalServerError . show) $
      usingReaderT conn $
      Database.update (unUserId (primaryKey user)) attributes
