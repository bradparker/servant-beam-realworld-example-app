module RealWorld.Conduit.Users.Web.Profiles.View
  ( View
  , handler
  , loadUserByUserName
  ) where

import Database.Beam (primaryKey)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.Decorated (Decorated(Decorated))
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Users.Web.Profile (Profile)
import qualified RealWorld.Conduit.Users.Web.Profile as Profile
import RealWorld.Conduit.Web.Auth (optionallyLoadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Capture, Handler(Handler))
import Servant.API ((:>), Get, JSON)
import Servant.Auth.Server (AuthResult(..))
import Servant.Auth.Swagger (Auth, JWT)

type Username = Text

type View =
  "api" :>
  "profiles" :>
  Capture "username" Username :>
  Auth '[JWT] Claim :>
  Get '[JSON] (Namespace "profile" Profile)

loadUserByUserName :: Environment -> Username -> Handler User
loadUserByUserName environment username =
  withDatabaseConnection environment $ \conn ->
    Handler $
    maybeToExceptT (notFound "User") $
    MaybeT $ Users.findByUsername conn username

queryFollowing :: Environment -> User -> User -> Handler Bool
queryFollowing environment follower followee =
  withDatabaseConnection environment $ \conn ->
    Handler $
    lift $ Users.following conn (primaryKey follower) (primaryKey followee)

handler ::
     Environment
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "profile" Profile)
handler environment username authResult = do
  currentUser <- optionallyLoadAuthorizedUser environment authResult
  profileUser <- loadUserByUserName environment username
  following <-
    maybe
      (pure False)
      (\user -> queryFollowing environment user profileUser)
      currentUser
  pure $ Namespace $ Profile.fromDecoratedUser $ Decorated profileUser following
