module RealWorld.Conduit.Spec
  ( withHandle
  ) where

import Control.Applicative (pure, (<*>))
import Data.Function (($), const)
import Data.Functor ((<$>))
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Spec.Database (withConnection)
import System.IO (IO)
import Web.JWT (secret)

createFakeConnectionPool :: Connection -> IO (Pool Connection)
createFakeConnectionPool conn = createPool (pure conn) (const (pure ())) 1 30 1

newFakeHandle :: Connection -> IO Handle
newFakeHandle conn =
  Handle
    <$> createFakeConnectionPool conn
    <*> pure (secret "unsafePerformIO")

withHandle :: (Handle -> IO a) -> IO a
withHandle action =
  withConnection $ \conn -> do
    handle <- newFakeHandle conn
    action handle
