module RealWorld.Conduit.Spec.Database
  ( withConnection
  ) where

import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Database.PostgreSQL.Simple (Connection, begin, close, rollback)
import RealWorld.Conduit.Database (openConduitDb)
import System.Environment (getEnv)

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket openConnection closeConnection
  where
    openConnection = do
      databaseUrl <- pack <$> getEnv "DATABASE_URL"
      conn <- openConduitDb databaseUrl
      begin conn
      pure conn
    closeConnection conn = do
      rollback conn
      close conn
