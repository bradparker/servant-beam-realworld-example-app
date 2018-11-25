module RealWorld.Conduit.Users.Web.Profiles.View
  ( View
  , handler
  , loadUserByUserName
  ) where

import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Profile (Profile)
import qualified RealWorld.Conduit.Users.Web.Profile as Profile
import RealWorld.Conduit.Web.Errors (notFound)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Capture, Handler(Handler))
import Servant.API ((:>), Get, JSON)

type Username = Text

type View =
  "api" :>
  "profiles" :>
  Capture "username" Username :>
  Get '[JSON] (Namespace "user" Profile)

loadUserByUserName :: Environment -> Username -> Handler User
loadUserByUserName environment username =
  withDatabaseConnection environment $ \conn ->
    Handler $
    maybeToExceptT (notFound "User") $ MaybeT $ Users.findByUsername conn username

handler :: Environment -> Username -> Handler (Namespace "user" Profile)
handler environment username =
  Namespace . Profile.fromUser <$> loadUserByUserName environment username
