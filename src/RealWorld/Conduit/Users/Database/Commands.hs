module RealWorld.Conduit.Users.Database.Commands
  ( create
  , update
  ) where

import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe, catMaybes, fromMaybe, listToMaybe)
import Database.Beam
  ( (<-.)
  , (==.)
  , default_
  , insertExpressions
  , primaryKey
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Database
  ( ConduitDb(conduitUsers)
  , conduitDb
  , insertOne
  )
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User (User, UserT(User))
import RealWorld.Conduit.Users.Database.User.Attributes (Attributes(..))
import System.IO (IO)

create :: Connection -> Attributes Identity -> IO User
create conn Attributes {password, email, username, bio, image} =
  insertOne
    conn
    (conduitUsers conduitDb)
    (insertExpressions
       [ User
           { User.id = default_
           , User.password = val_ password
           , User.email = val_ email
           , User.bio = val_ bio
           , User.username = val_ username
           , User.image = val_ image
           }
       ])

update :: Connection -> User -> Attributes Maybe -> IO User
update conn currentUser Attributes {password, email, username, bio, image} =
  fromMaybe currentUser . listToMaybe <$>
  runBeamPostgres
    conn
    (runUpdateReturningList
       (conduitUsers conduitDb)
       (\user ->
          catMaybes
            [ (User.password user <-.) . val_ <$> password
            , (User.email user <-.) . val_ <$> email
            , (User.username user <-.) . val_ <$> username
            , (User.bio user <-.) . val_ <$> bio
            , (User.image user <-.) . val_ <$> image
            ])
       (\user -> primaryKey user ==. val_ (primaryKey currentUser)))
