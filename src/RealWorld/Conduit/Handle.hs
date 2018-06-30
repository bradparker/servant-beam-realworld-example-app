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
import System.IO (IO)
import Web.JWT (Secret)

data Handle = Handle
  { connectionPool :: Pool Connection
  , authSecret :: Secret
  }

createConnectionPool :: Options -> IO (Pool Connection)
createConnectionPool options =
  createPool (openConduitDb (Options.databaseUrl options)) close 1 10 8

new :: Options -> IO Handle
new options =
  Handle
    <$> createConnectionPool options
    <*> pure (Options.authSecret options)
