module RealWorld.Conduit.Spec
  ( withHandle
  ) where

import Data.Function (($), (.))
import Data.List (replicate)
import Data.String (String)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Options (octKey)
import RealWorld.Conduit.Spec.Database (withConnection)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import System.IO (IO)

createFakeJWTSettings :: String -> JWTSettings
createFakeJWTSettings = defaultJWTSettings . octKey

newFakeHandle :: Connection -> Handle
newFakeHandle conn =
  Handle
    { jwtSettings = createFakeJWTSettings (replicate 256 'X')
    , withDatabaseConnection = ($ conn)
    }

withHandle :: (Handle -> IO a) -> IO a
withHandle action = withConnection (action . newFakeHandle)
