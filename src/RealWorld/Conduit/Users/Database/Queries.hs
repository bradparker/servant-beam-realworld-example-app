module RealWorld.Conduit.Users.Database.Queries
  ( find
  , findByEmail
  , findByUsername
  , findByCredentials
  , followersAndFollowees
  ) where

import Prelude hiding (find)
import Crypto.Scrypt (EncryptedPass(EncryptedPass), Pass(Pass), verifyPass')
import Database.Beam (Q, QExpr, all_, manyToMany_)
import Database.Beam.Postgres.Syntax (PgExpressionSyntax, PgSelectSyntax)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Database (ConduitDb(conduitUsers, conduitFollows), conduitDb, findBy)
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import qualified RealWorld.Conduit.Users.Database.Credentials as Credentials
import qualified RealWorld.Conduit.Users.Database.Follow as Follow
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User
  ( PrimaryKey(unUserId)
  , UserId
  , User
  , UserT
  )

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
