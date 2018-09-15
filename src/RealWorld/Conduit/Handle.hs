{-# LANGUAGE RankNTypes #-}

module RealWorld.Conduit.Handle
  ( Handle(..)
  , new
  ) where

import Control.Applicative (pure)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple (Connection, close)
import RealWorld.Conduit.Database (openConduitDb)
import RealWorld.Conduit.Options (Options)
import qualified RealWorld.Conduit.Options as Options
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import System.IO (IO)

data Handle = Handle
  { jwtSettings :: JWTSettings
  , withDatabaseConnection :: forall a m. MonadBaseControl IO m =>
                                            (Connection -> m a) -> m a
  }

createDatabaseConnectionPool :: Options -> IO (Pool Connection)
createDatabaseConnectionPool options =
  createPool (openConduitDb (Options.databaseUrl options)) close 1 10 8

new :: Options -> IO Handle
new options = do
  databaseConnectionPool <- createDatabaseConnectionPool options
  pure
    Handle
      { jwtSettings = defaultJWTSettings (Options.authKey options)
      , withDatabaseConnection = withResource databaseConnectionPool
      }
