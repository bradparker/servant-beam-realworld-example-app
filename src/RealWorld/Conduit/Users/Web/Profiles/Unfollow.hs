module RealWorld.Conduit.Users.Web.Profiles.Unfollow
  ( Unfollow
  , handler
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Users.Web.Profile (Profile)
import qualified RealWorld.Conduit.Users.Web.Profile as Profile
import RealWorld.Conduit.Users.Web.Profiles.View (loadUserByUserName)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Capture, Handler(Handler))
import Servant.API ((:>), Delete, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Username = Text

type Unfollow =
  "api" :>
  "profiles" :>
  Capture "username" Username :>
  "follow" :>
  Auth '[JWT] Claim :>
  Delete '[JSON] (Namespace "user" Profile)

deleteFollow :: Environment -> User -> User -> Handler ()
deleteFollow environment follower followee =
  Handler $
  withDatabaseConnection environment $ \conn ->
    lift $ void $ Users.unfollow conn (primaryKey follower) (primaryKey followee)

handler ::
     Environment
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "user" Profile)
handler environment username authresult = do
  user <- loadAuthorizedUser environment authresult
  profile <- loadUserByUserName environment username
  Namespace (Profile.fromUser profile) <$ deleteFollow environment user profile
