module RealWorld.Conduit.Users.Web.Current.View
  ( View
  , handler
  ) where

import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.Text as Text
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId))
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Auth (withRequiredAuth)
import RealWorld.Conduit.Web.Errors (internalServerError, notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr)
import Servant.API ((:>), Get, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import System.IO (IO)
import Text.Show (show)

type View =
  Auth '[JWT] Claim :>
  "api" :>
  "user" :>
  Get '[JSON] (Namespace "user" Account)

handler :: Handle -> AuthResult Claim -> Handler (Namespace "user" Account)
handler handle = withRequiredAuth $ \claim ->
  Handler $ Namespace <$> view handle claim

view :: Handle -> Claim -> ExceptT ServantErr IO Account
view Handle {withDatabaseConnection, jwtSettings} (Claim id) =
  withDatabaseConnection $ \conn -> do
    user <-
      maybeToExceptT (notFound "User") $ MaybeT $ Database.find conn (UserId id)
    withExceptT (internalServerError . Text.pack . show) $
      fromUser jwtSettings user
