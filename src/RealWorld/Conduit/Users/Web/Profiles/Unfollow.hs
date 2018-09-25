module RealWorld.Conduit.Users.Web.Profiles.Unfollow
  ( Unfollow
  , handler
  ) where

import Control.Monad.Trans.Class (lift)
import Data.Function (($))
import Data.Functor ((<$), void)
import Data.Text (Text)
import Database.Beam (primaryKey)
import RealWorld.Conduit.Handle (Handle(..))
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

deleteFollow :: Handle -> User -> User -> Handler ()
deleteFollow handle follower followee =
  Handler $
  withDatabaseConnection handle $ \conn ->
    lift $ void $ Users.unfollow conn (primaryKey follower) (primaryKey followee)

handler ::
     Handle
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "user" Profile)
handler handle username authresult = do
  user <- loadAuthorizedUser handle authresult
  profile <- loadUserByUserName handle username
  Namespace (Profile.fromUser profile) <$ deleteFollow handle user profile
