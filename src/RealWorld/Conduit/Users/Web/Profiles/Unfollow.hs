module RealWorld.Conduit.Users.Web.Profiles.Unfollow
  ( Unfollow
  , handler
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (UserId, PrimaryKey(UserId))
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Users.Profile (Profile)
import qualified RealWorld.Conduit.Users.Profile as Profile
import RealWorld.Conduit.Users.Web.Profiles.View (loadProfile)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Capture, Handler)
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
  Delete '[JSON] (Namespace "profile" Profile)

deleteFollow :: Environment -> UserId -> UserId -> Handler ()
deleteFollow environment follower followee =
  withDatabaseConnection environment $
  runReaderT $ Users.unfollow follower followee

handler ::
     Environment
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "profile" Profile)
handler environment username authresult = do
  currentUserId <- primaryKey <$> loadAuthorizedUser environment authresult
  profile <- loadProfile environment (Just currentUserId) username
  deleteFollow environment currentUserId (UserId . Profile.id $ profile)
  Namespace <$> loadProfile environment (Just currentUserId) username
