module RealWorld.Conduit.Tags.Database
  ( create
  , query
  , TagT(..)
  , Tag
  , TagId
  ) where

import Conduit (sourceToList)
import Database.Beam
  ( all_
  , insertExpressions
  , runSelectReturningList
  , select
  , val_
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Conduit (runInsertReturning)
import Database.Beam.Postgres.Full
  ( conflictingFields
  , insertReturning
  , onConflict
  , onConflictUpdateInstead
  )
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Database (ConduitDb(..), conduitDb)
import RealWorld.Conduit.Tags.Database.Tag (Tag, TagId, TagT(Tag))
import qualified RealWorld.Conduit.Tags.Database.Tag as Tag

query :: Connection -> IO [Tag]
query conn =
  runBeamPostgres conn $
  runSelectReturningList $ select $ all_ $ conduitTags conduitDb

create :: Connection -> Set Text -> IO [Tag]
create conn names =
  runInsertReturning
    conn
    (insertReturning
       (conduitTags conduitDb)
       (insertExpressions (map (Tag . val_) (toList names)))
       (onConflict (conflictingFields Tag.name) (onConflictUpdateInstead Tag.name))
       (Just id))
    sourceToList
