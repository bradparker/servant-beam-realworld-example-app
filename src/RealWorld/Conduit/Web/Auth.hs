module RealWorld.Conduit.Web.Auth
  ( loadAuthorizedUser
  ) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($))
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId), User)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Errors (notAuthorized)
import Servant (Handler(Handler), throwError)
import Servant.Auth.Server (AuthResult(..))

loadAuthorizedUser :: Handle -> AuthResult Claim -> Handler User
loadAuthorizedUser handle (Authenticated (Claim id)) =
  withDatabaseConnection handle $ \conn ->
    Handler $
    maybeToExceptT notAuthorized $
    MaybeT $ Users.find conn (UserId id)
loadAuthorizedUser _ _ = throwError notAuthorized
