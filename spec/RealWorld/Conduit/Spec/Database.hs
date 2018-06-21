module RealWorld.Conduit.Spec.Database
  ( withConnection
  ) where

import RealWorld.Conduit.Database (openConduitDb)
import Control.Applicative (pure)
import Control.Exception (bracket)
import Database.PostgreSQL.Simple (Connection, begin, close, rollback)
import System.IO (IO)

withConnection :: (Connection -> IO a) -> IO a
withConnection =
  bracket
    (do
      conn <- openConduitDb
      begin conn
      pure conn)
    (\conn -> do
      rollback conn
      close conn)
