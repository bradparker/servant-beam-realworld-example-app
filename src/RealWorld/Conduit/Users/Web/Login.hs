module RealWorld.Conduit.Users.Web.Login
  ( Login
  , handler
  ) where

import Control.Monad.Trans.Except (withExceptT)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Errors (internalServerError, notAuthorized)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler))
import Servant.API ((:>), JSON, Post, ReqBody)

type Login =
  "api" :>
  "users" :>
  "login" :>
  ReqBody '[JSON] (Namespace "user" Credentials) :>
  Post '[JSON] (Namespace "user" Account)

handler ::
     Environment
  -> Namespace "user" Credentials
  -> Handler (Namespace "user" Account)
handler environment (Namespace params) =
  Namespace <$> login environment params

login :: Environment -> Credentials -> Handler Account
login Environment {withDatabaseConnection, jwtSettings} creds =
  Handler $
  withDatabaseConnection $ \conn -> do
    user <-
      maybeToExceptT notAuthorized $ MaybeT $ usingReaderT conn $ Database.findByCredentials creds
    withExceptT (internalServerError . show) $ fromUser jwtSettings user
