module RealWorld.Conduit.Users.Web.Current.View
  ( View
  , handler
  ) where

import RealWorld.Conduit.Environment (Environment(..))
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

handler :: Environment -> AuthResult Claim -> Handler (Namespace "user" Account)
handler environment authresult = do
  user <- loadAuthorizedUser environment authresult
  Namespace <$> account environment user
