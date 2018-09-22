module RealWorld.Conduit.Users.Web.Current.View
  ( View
  , handler
  ) where

import Data.Functor ((<$>))
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Users.Web.Account (Account, account)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler)
import Servant.API ((:>), Get, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type View =
  "api" :>
  "user" :>
  Auth '[JWT] Claim :>
  Get '[JSON] (Namespace "user" Account)

handler :: Handle -> AuthResult Claim -> Handler (Namespace "user" Account)
handler handle authresult = do
  user <- loadAuthorizedUser handle authresult
  Namespace <$> account handle user
