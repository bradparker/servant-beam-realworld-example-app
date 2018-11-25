module RealWorld.Conduit.Spec
  ( withEnvironment
  ) where

import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Options (octKey)
import RealWorld.Conduit.Spec.Database (withConnection)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)

createFakeJWTSettings :: String -> JWTSettings
createFakeJWTSettings = defaultJWTSettings . octKey

newFakeEnvironment :: Connection -> Environment
newFakeEnvironment conn =
  Environment
    { jwtSettings = createFakeJWTSettings (replicate 256 'X')
    , withDatabaseConnection = ($ conn)
    }

withEnvironment :: (Environment -> IO a) -> IO a
withEnvironment action = withConnection (action . newFakeEnvironment)
