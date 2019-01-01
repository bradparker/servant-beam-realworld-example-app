module RealWorld.Conduit.Web.Auth
  ( loadAuthorizedUser
  , optionallyLoadAuthorizedUser
  ) where

import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Users
import RealWorld.Conduit.Users.Database.User (User)
import RealWorld.Conduit.Users.Web.Claim (Claim(Claim))
import RealWorld.Conduit.Web.Errors (notAuthorized)
import Servant (Handler(Handler), throwError)
import Servant.Auth.Server (AuthResult(..))

loadAuthorizedUser :: Environment -> AuthResult Claim -> Handler User
loadAuthorizedUser environment authResult =
  case authResult of
    Authenticated (Claim identifier) ->
      withDatabaseConnection environment $ \conn ->
        Handler $
        maybeToExceptT notAuthorized $
        MaybeT $ runReaderT (Users.find identifier) conn
    _ -> throwError notAuthorized

optionallyLoadAuthorizedUser :: Environment -> AuthResult Claim -> Handler (Maybe User)
optionallyLoadAuthorizedUser environment authResult =
  case authResult of
    Indefinite -> pure Nothing
    _ -> Just <$> loadAuthorizedUser environment authResult
