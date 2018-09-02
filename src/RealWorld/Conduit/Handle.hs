module RealWorld.Conduit.Handle
  ( Handle(..)
  , new
  ) where

import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close)
import RealWorld.Conduit.Database (openConduitDb)
import RealWorld.Conduit.Options (Options)
import qualified RealWorld.Conduit.Options as Options
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import System.IO (IO)

data Handle = Handle
  { connectionPool :: Pool Connection
  , jwtSettings :: JWTSettings
  }

createConnectionPool :: Options -> IO (Pool Connection)
createConnectionPool options =
  createPool (openConduitDb (Options.databaseUrl options)) close 1 10 8

new :: Options -> IO Handle
new options =
  Handle
    <$> createConnectionPool options
    <*> pure (defaultJWTSettings (Options.authKey options))
