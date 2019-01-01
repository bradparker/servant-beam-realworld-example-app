module RealWorld.Conduit.Users.Web.Profiles.Follow
  ( Follow
  , handler
  ) where

import Control.Monad.Except (withExceptT)
import Database.Beam (primaryKey)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId), UserId)
import RealWorld.Conduit.Users.Profile (Profile)
import qualified RealWorld.Conduit.Users.Profile as Profile
import RealWorld.Conduit.Users.Web.Claim (Claim)
import RealWorld.Conduit.Users.Web.Profiles.View (loadProfile)
import RealWorld.Conduit.Web.Auth (loadAuthorizedUser)
import RealWorld.Conduit.Web.Errors (internalServerError)
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
  Post '[JSON] (Namespace "profile" Profile)

createFollow :: Environment -> UserId -> UserId -> Handler ()
createFollow environment followerId followeeId =
  Handler $ void $ withDatabaseConnection environment $
    withExceptT (internalServerError . show) .
    runReaderT (Database.follow followerId followeeId)

handler ::
     Environment
  -> Username
  -> AuthResult Claim
  -> Handler (Namespace "profile" Profile)
handler environment username authresult = do
  currentUserId <- primaryKey <$> loadAuthorizedUser environment authresult
  profile <- loadProfile environment (Just currentUserId) username
  createFollow environment currentUserId (UserId . Profile.id $ profile)
  Namespace <$> loadProfile environment (Just currentUserId) username
