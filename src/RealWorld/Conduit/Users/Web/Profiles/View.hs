module RealWorld.Conduit.Users.Web.Profiles.View
  ( View
  , handler
  , loadUserByUserName
  ) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Text (Text)
import RealWorld.Conduit.Handle (Handle(..))
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

loadUserByUserName :: Handle -> Username -> Handler User
loadUserByUserName handle username =
  withDatabaseConnection handle $ \conn ->
    Handler $
    maybeToExceptT (notFound "User") $ MaybeT $ Users.findByUsername conn username

handler :: Handle -> Username -> Handler (Namespace "user" Profile)
handler handle username =
  Namespace . Profile.fromUser <$> loadUserByUserName handle username
