module RealWorld.Conduit.Users.Database.Queries
  ( findByEmail
  , findByUsername
  ) where

import Data.Maybe (Maybe)
import Data.Text (Text)
import Database.Beam (all_)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Database (ConduitDb(conduitUsers), conduitDb, findBy)
import RealWorld.Conduit.Users.Database.User (User, UserT(..))
import System.IO (IO)

findByEmail :: Connection -> Text -> IO (Maybe User)
findByEmail conn = findBy conn (all_ (conduitUsers conduitDb)) email

findByUsername :: Connection -> Text -> IO (Maybe User)
findByUsername conn = findBy conn (all_ (conduitUsers conduitDb)) username
