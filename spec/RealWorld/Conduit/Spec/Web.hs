module RealWorld.Conduit.Spec.Web
  ( withApp
  , authHeader
  ) where

import Network.HTTP.Types (Header, hAuthorization)
import Network.Wai (Application)
import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Spec (withEnvironment)
import RealWorld.Conduit.Users.Web.Account (Account)
import qualified RealWorld.Conduit.Users.Web.Account as Account
import Test.Hspec (ActionWith)

withApp :: (Environment -> Application) -> ActionWith Application -> IO ()
withApp app action = withEnvironment (action . app)

authHeader :: Account -> Header
authHeader account = (hAuthorization, encodeUtf8 $ "Bearer " <> Account.token account)
