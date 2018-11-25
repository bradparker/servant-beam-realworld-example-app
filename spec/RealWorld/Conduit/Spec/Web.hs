module RealWorld.Conduit.Spec.Web
  ( withApp
  ) where

import RealWorld.Conduit.Environment (Environment)
import Network.Wai (Application)
import RealWorld.Conduit.Spec (withEnvironment)
import Test.Hspec (ActionWith)

withApp :: (Environment -> Application) -> ActionWith Application -> IO ()
withApp app action = withEnvironment (action . app)
