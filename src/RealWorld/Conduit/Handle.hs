module RealWorld.Conduit.Handle
  ( Handle(..)
  , new
  ) where

import Data.Functor ((<$>))
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close)
import RealWorld.Conduit.Database (openConduitDb)
import RealWorld.Conduit.Options (Options(..))
import System.IO (IO)

newtype Handle = Handle
  { connectionPool :: Pool Connection
  }

createConnectionPool :: Options -> IO (Pool Connection)
createConnectionPool options = createPool (openConduitDb options) close 1 10 8

new :: Options -> IO Handle
new options = Handle <$> createConnectionPool options
