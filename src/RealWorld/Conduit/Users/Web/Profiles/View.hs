module RealWorld.Conduit.Users.Web.Profiles.View
  ( View
  , handler
  , loadProfile
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (UserId)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Web.Auth (optionallyLoadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Capture, Handler(Handler))
import Servant.API ((:>), Get, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)
import RealWorld.Conduit.Users.Profile (Profile)

type Username = Text

type View =
  "api" :>
  "profiles" :>
  Capture "username" Username :>
  Auth '[JWT] Claim :>
  Get '[JSON] (Namespace "profile" Profile)

loadProfile :: Environment -> Maybe UserId -> Text -> Handler Profile
loadProfile environment currentUserId username =
  withDatabaseConnection environment
    $ Handler
    . maybeToExceptT (notFound "Profile")
    . MaybeT
    . runReaderT (Database.findProfile currentUserId username)

handler
  :: Environment
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "profile" Profile)
handler environment username authResult = do
  currentUser <- optionallyLoadAuthorizedUser environment authResult
  Namespace <$> loadProfile environment (primaryKey <$> currentUser) username
