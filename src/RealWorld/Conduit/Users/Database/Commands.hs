module RealWorld.Conduit.Users.Database.Commands
  ( create
  , update
  , follow
  , unfollow
  ) where

import Database.Beam
  ( (&&.)
  , (<-.)
  , (==.)
  , default_
  , delete
  , insertExpressions
  , primaryKey
  , runDelete
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Database
  ( ConduitDb(conduitFollows, conduitUsers)
  , conduitDb
  , insertOne
  )
import RealWorld.Conduit.Users.Database.Follow (Follow, FollowT(Follow))
import qualified RealWorld.Conduit.Users.Database.Follow as Follow
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User (User, UserId, UserT(User))
import RealWorld.Conduit.Users.Database.User.Attributes (Attributes(..))

create :: Connection -> Attributes Identity -> IO User
create conn Attributes {password, email, username, bio, image} =
  insertOne conn (conduitUsers conduitDb) $
  insertExpressions
    [ User
        { User.id = default_
        , User.password = val_ password
        , User.email = val_ email
        , User.bio = val_ bio
        , User.username = val_ username
        , User.image = val_ image
        }
    ]

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

follow :: Connection -> UserId -> UserId -> IO Follow
follow conn followerId followeeId =
  insertOne conn (conduitFollows conduitDb) $
  insertExpressions
    [ Follow
        { Follow.follower = val_ followerId
        , Follow.followee = val_ followeeId
        }
    ]

unfollow :: Connection -> UserId -> UserId -> IO ()
unfollow conn followerId followeeId =
  runBeamPostgres conn $
  runDelete $
  delete (conduitFollows conduitDb) $ \(Follow follower followee) ->
    follower ==. val_ followerId &&. followee ==. val_ followeeId
