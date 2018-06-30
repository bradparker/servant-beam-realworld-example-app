module RealWorld.Conduit.Spec.Web
  ( withApp
  ) where

import RealWorld.Conduit.Handle (Handle)
import Network.Wai (Application)
import RealWorld.Conduit.Spec (withHandle)
import Test.Hspec (ActionWith)
import System.IO (IO)
import Data.Function ((.))

withApp :: (Handle -> Application) -> ActionWith Application -> IO ()
withApp app action = withHandle (action . app)
