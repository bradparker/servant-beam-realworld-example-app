module RealWorld.Conduit.Web.Auth
  ( loadAuthorizedUser
  , optionallyLoadAuthorizedUser
  ) where

import Control.Applicative (pure)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (PrimaryKey(UserId), User)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Errors (notAuthorized)
import Servant (Handler(Handler), throwError)
import Servant.Auth.Server (AuthResult(..))

loadAuthorizedUser :: Handle -> AuthResult Claim -> Handler User
loadAuthorizedUser handle authResult =
  case authResult of
    Authenticated (Claim id) ->
      withDatabaseConnection handle $ \conn ->
        Handler $
        maybeToExceptT notAuthorized $
        MaybeT $ Users.find conn (UserId id)
    _ -> throwError notAuthorized

optionallyLoadAuthorizedUser :: Handle -> AuthResult Claim -> Handler (Maybe User)
optionallyLoadAuthorizedUser handle authResult =
  case authResult of
    Indefinite -> pure Nothing
    _ -> Just <$> loadAuthorizedUser handle authResult
