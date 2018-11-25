module RealWorld.Conduit.Users.Web.Profiles.Follow
  ( Follow
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
import Servant.API ((:>), JSON, Post)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Username = Text

type Follow =
  "api" :>
  "profiles" :>
  Capture "username" Username :>
  "follow" :>
  Auth '[JWT] Claim :>
  Post '[JSON] (Namespace "user" Profile)

createFollow :: Environment -> User -> User -> Handler ()
createFollow environment follower followee =
  Handler $
  withDatabaseConnection environment $ \conn ->
    lift $ void $ Users.follow conn (primaryKey follower) (primaryKey followee)

handler ::
     Environment
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "user" Profile)
handler environment username authresult = do
  user <- loadAuthorizedUser environment authresult
  profile <- loadUserByUserName environment username
  Namespace (Profile.fromUser profile) <$ createFollow environment user profile
