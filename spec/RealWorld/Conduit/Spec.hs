module RealWorld.Conduit.Spec
  ( withHandle
  ) where

import Control.Applicative ((<*>), pure)
import Data.Function (($), (.), const)
import Data.Functor ((<$>))
import Data.List (replicate)
import Data.Pool (Pool, createPool)
import Data.String (String)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Options (octKey)
import RealWorld.Conduit.Spec.Database (withConnection)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import System.IO (IO)

createFakeConnectionPool :: Connection -> IO (Pool Connection)
createFakeConnectionPool conn = createPool (pure conn) (const (pure ())) 1 30 1

createFakeJWTSettings :: String -> JWTSettings
createFakeJWTSettings = defaultJWTSettings . octKey

newFakeHandle :: Connection -> IO Handle
newFakeHandle conn =
  Handle
    <$> createFakeConnectionPool conn
    <*> pure (createFakeJWTSettings (replicate 256 'X'))

withHandle :: (Handle -> IO a) -> IO a
withHandle action =
  withConnection $ \conn -> do
    handle <- newFakeHandle conn
    action handle
