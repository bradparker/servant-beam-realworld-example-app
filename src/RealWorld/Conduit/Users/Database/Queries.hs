module RealWorld.Conduit.Users.Database.Queries
  ( find
  , findByEmail
  , findByUsername
  , findByCredentials
  , followersAndFollowees
  , follows
  , following
  ) where

import Crypto.Scrypt (EncryptedPass(EncryptedPass), Pass(Pass), verifyPass')
import qualified Data.Foldable as Foldable
import Database.Beam
  ( Nullable
  , Q
  , QExpr
  , (&&.)
  , (==.)
  , all_
  , filter_
  , leftJoin_
  , manyToMany_
  , primaryKey
  , references_
  , runSelectReturningList
  , select
  , val_
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Syntax (PgExpressionSyntax, PgSelectSyntax)
import Database.PostgreSQL.Simple (Connection)
import Prelude hiding (find)
import RealWorld.Conduit.Database
  ( ConduitDb(conduitFollows, conduitUsers)
  , conduitDb
  , findBy
  )
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import qualified RealWorld.Conduit.Users.Database.Credentials as Credentials
import RealWorld.Conduit.Users.Database.Follow (FollowT)
import qualified RealWorld.Conduit.Users.Database.Follow as Follow
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User
  ( PrimaryKey(unUserId)
  , User
  , UserId
  , UserT
  )

type QueryExpression s = QExpr PgExpressionSyntax s

find :: Connection -> UserId -> IO (Maybe User)
find conn = findBy conn (all_ (conduitUsers conduitDb)) User.id . unUserId

findByEmail :: Connection -> Text -> IO (Maybe User)
findByEmail conn = findBy conn (all_ (conduitUsers conduitDb)) User.email

findByUsername :: Connection -> Text -> IO (Maybe User)
findByUsername conn = findBy conn (all_ (conduitUsers conduitDb)) User.username

encryptedPassMatches :: Text -> Text -> Bool
encryptedPassMatches a b =
  verifyPass' (Pass (encodeUtf8 a)) (EncryptedPass (encodeUtf8 b))

findByCredentials :: Connection -> Credentials -> IO (Maybe User)
findByCredentials conn credentials = do
  found <- findByEmail conn (Credentials.email credentials)
  if maybe False (encryptedPassMatches (Credentials.password credentials) . User.password) found
    then pure found
    else pure Nothing

followersAndFollowees ::
     Q PgSelectSyntax ConduitDb s ( UserT (QExpr PgExpressionSyntax s)
                                  , UserT (QExpr PgExpressionSyntax s))
followersAndFollowees =
  manyToMany_
    (conduitFollows conduitDb)
    Follow.follower
    Follow.followee
    (all_ (conduitUsers conduitDb))
    (all_ (conduitUsers conduitDb))

follows ::
  UserT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (FollowT (Nullable (QueryExpression s)))
follows author =
  leftJoin_
    (all_ (conduitFollows conduitDb))
    ((`references_` author) . Follow.followee)

following :: Connection -> UserId -> UserId -> IO Bool
following conn a b =
  not . Foldable.null <$>
  runBeamPostgres
    conn
    (runSelectReturningList $
     select $
     filter_
       (\(follower, followee) ->
          primaryKey follower ==. val_ a &&. primaryKey followee ==. val_ b)
       followersAndFollowees)
