module RealWorld.Conduit.Users.Database
  ( ProfileResult
  , ProfileRow
  , attributesForInsert
  , attributesForUpdate
  , create
  , find
  , findByCredentials
  , findProfile
  , follow
  , selectProfiles
  , unfollow
  , update
  ) where

import Control.Lens ((^.), _1, _2, view)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.Scrypt
  ( EncryptedPass(EncryptedPass, getEncryptedPass)
  , Pass(Pass)
  , encryptPassIO'
  , verifyPass'
  )
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Validation (Validation(Failure, Success), validation)
import Database.Beam.Postgres.Extended
  ( HasSqlEqualityCheck
  , Nullable
  , PgExpressionSyntax
  , PgInsertReturning
  , PgQExpr
  , PgSelectSyntax
  , PgUpdateReturning
  , Q
  , (&&.)
  , (<-.)
  , (==.)
  , aggregate_
  , all_
  , default_
  , delete
  , exists_
  , group_
  , guard_
  , insertExpressions
  , insertReturning
  , just_
  , leftJoin_
  , onConflictDefault
  , pgBoolOr
  , primaryKey
  , references_
  , runDelete
  , runInsertReturning
  , runSelect
  , runUpdateReturning
  , select
  , updateReturning
  , val_
  )
import Database.PostgreSQL.Simple (Connection)
import Prelude hiding (find)
import RealWorld.Conduit.Database
  ( ConduitDb(conduitFollows, conduitUsers)
  , QueryError
  , conduitDb
  , maybeRow
  , singleRow
  )
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import qualified RealWorld.Conduit.Users.Database.Credentials as Credentials
import RealWorld.Conduit.Users.Database.Follow (Follow, FollowT(Follow))
import qualified RealWorld.Conduit.Users.Database.Follow as Follow
import qualified RealWorld.Conduit.Users.Database.User as User
import RealWorld.Conduit.Users.Database.User
  ( PrimaryKey(UserId)
  , User
  , UserId
  , UserT(User)
  )
import RealWorld.Conduit.Users.Profile (Profile(Profile))
import RealWorld.Conduit.Users.User.Attributes (Attributes(Attributes))
import qualified RealWorld.Conduit.Users.User.Attributes as Attributes

insertUser
  :: Attributes Identity -> PgInsertReturning User
insertUser attributes
  = insertReturning
    (conduitUsers conduitDb)
    (insertExpressions
      [ User
          { User.id = default_
          , User.password = val_ (Attributes.password attributes)
          , User.email = val_ (Attributes.email attributes)
          , User.bio = val_ (Attributes.bio attributes)
          , User.username = val_ (Attributes.username attributes)
          , User.image = val_ (Attributes.image attributes)
          }
      ]
    )
    onConflictDefault
    (Just id)

create
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => Attributes Identity
  -> m User
create attributes = do
  conn <- ask
  runInsertReturning conn (insertUser attributes) singleRow

updateUser
  :: UserId -> Attributes Maybe -> PgUpdateReturning User
updateUser userId attributes
  = updateReturning
    (conduitUsers conduitDb)
    (\user -> catMaybes
      [ (User.password user <-.) . val_ <$> Attributes.password attributes
      , (User.email user <-.) . val_ <$> Attributes.email attributes
      , (User.username user <-.) . val_ <$> Attributes.username attributes
      , (User.bio user <-.) . val_ <$> Attributes.bio attributes
      , (User.image user <-.) . val_ <$> Attributes.image attributes
      ]
    )
    ((val_ userId ==.) . primaryKey)
    id

update
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => Int
  -> Attributes Maybe
  -> m User
update userId attributes = do
  conn <- ask
  runUpdateReturning
    conn
    (updateUser (UserId userId) attributes)
    singleRow

follow
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError QueryError m
     )
  => UserId
  -> UserId
  -> m Follow
follow followerId followeeId = do
  conn <- ask
  runInsertReturning
    conn
    (insertReturning
      (conduitFollows conduitDb)
      (insertExpressions
        [ Follow
            { Follow.follower = val_ followerId
            , Follow.followee = val_ followeeId
            }
        ]
      )
      onConflictDefault
      (Just id)
    )
    singleRow

unfollow
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => UserId
  -> UserId
  -> m ()
unfollow followerId followeeId = do
  conn <- ask
  void
    $ runDelete conn
    $ delete (conduitFollows conduitDb)
    $ \(Follow follower followee) ->
        follower ==. val_ followerId &&. followee ==. val_ followeeId

type ValidationErrors = Map Text [Text]

encryptPassword :: Text -> IO Text
encryptPassword password =
  decodeUtf8 . getEncryptedPass <$> encryptPassIO' (Pass (encodeUtf8 password))

makePassword :: MonadIO m => Text -> Compose m (Validation ValidationErrors) Text
makePassword value
  | Text.length value < 8 = Compose $ pure $ Failure
    (Map.singleton "password" ["Must be longer than 8 chars"])
  | otherwise = Compose $ Success <$> liftIO (encryptPassword value)

usernameExists
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> m Bool
usernameExists username = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ do
      user <- all_ (conduitUsers conduitDb)
      guard_ (User.username user ==. val_ username)
      pure user

emailExists
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> m Bool
emailExists email = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ do
      user <- all_ (conduitUsers conduitDb)
      guard_ (User.email user ==. val_ email)
      pure user

makeEmail
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> Compose m (Validation ValidationErrors) Text
makeEmail value =
  Compose $ do
    exists <- emailExists value
    pure $
      if exists
        then Failure (Map.singleton "email" ["Taken"])
        else Success value

makeUsername
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> Compose m (Validation ValidationErrors) Text
makeUsername value =
  Compose $ do
    exists <- usernameExists value
    pure $
      if exists
        then Failure (Map.singleton "username" ["Taken"])
        else Success value

attributesForInsert
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadError ValidationErrors m
     )
  => Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> m (Attributes Identity)
attributesForInsert passwordVal emailVal usernameVal bioVal imageVal =
  (validation throwError pure =<<) . getCompose $
  Attributes
    <$> makePassword passwordVal
    <*> makeEmail emailVal
    <*> makeUsername usernameVal
    <*> pure bioVal
    <*> pure imageVal

makeUpdateEmail
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => User
  -> Text
  -> Compose m (Validation ValidationErrors) Text
makeUpdateEmail current value
  | value == User.email current = pure value
  | otherwise = makeEmail value

makeUpdateUsername
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => User
  -> Text
  -> Compose m (Validation ValidationErrors) Text
makeUpdateUsername current value
  | value == User.username current = pure value
  | otherwise = makeUsername value

attributesForUpdate
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadError ValidationErrors m
     )
  => User
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Maybe Text)
  -> m (Attributes Maybe)
attributesForUpdate current passwordVal emailVal usernameVal bioVal imageVal =
  (validation throwError pure =<<) . getCompose $
  Attributes
    <$> traverse makePassword passwordVal
    <*> traverse (makeUpdateEmail current) emailVal
    <*> traverse (makeUpdateUsername current) usernameVal
    <*> pure bioVal
    <*> pure imageVal

selectUserBy
  :: HasSqlEqualityCheck PgExpressionSyntax a
  => (UserT (PgQExpr s) -> PgQExpr s a)
  -> PgQExpr s a
  -> Q PgSelectSyntax ConduitDb s (UserT (PgQExpr s))
selectUserBy f a = do
  user <- all_ (conduitUsers conduitDb)
  guard_ (f user ==. a)
  pure user

find
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Int
  -> m (Maybe User)
find userId = do
  conn <- ask
  runSelect conn (select (selectUserBy User.id (val_ userId))) maybeRow

findByEmail
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Text
  -> m (Maybe User)
findByEmail email = do
  conn <- ask
  runSelect conn (select (selectUserBy User.email (val_ email))) maybeRow

encryptedPassMatches :: Text -> Text -> Bool
encryptedPassMatches a b =
  verifyPass' (Pass (encodeUtf8 a)) (EncryptedPass (encodeUtf8 b))

findByCredentials
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Credentials
  -> m (Maybe User)
findByCredentials credentials = do
  found <- findByEmail (Credentials.email credentials)
  if maybe False (encryptedPassMatches (Credentials.password credentials) . User.password) found
    then pure found
    else pure Nothing

follows
  :: UserT (PgQExpr s)
  -> Q PgSelectSyntax ConduitDb s (FollowT (Nullable (PgQExpr s)))
follows author =
  leftJoin_
    (all_ (conduitFollows conduitDb))
    ((`references_` author) . Follow.followee)

type ProfileRow s =
  ( UserT (PgQExpr s)
  , PgQExpr s (Maybe Bool)
  )

type ProfileResult =
  ( User
  , Maybe Bool
  )

selectProfiles
  :: Maybe UserId
  -> Q PgSelectSyntax ConduitDb s (ProfileRow s)
selectProfiles currentUserId =
  aggregate_ (\(user, following) -> (group_ user, pgBoolOr following)) $ do
    user <- all_ (conduitUsers conduitDb)
    follow' <- follows user
    pure
      ( user
      , maybe
          (val_ False)
          ((Follow.follower follow' ==.) . just_ . val_)
          currentUserId)

selectProfile
  :: Maybe UserId
  -> Text
  -> Q PgSelectSyntax ConduitDb s (ProfileRow s)
selectProfile currentUserId username = do
  profile <- selectProfiles currentUserId
  guard_ $ User.username (profile ^. _1) ==. val_ username
  pure profile

toProfile :: ProfileResult -> Profile
toProfile =
  Profile
    <$> (User.id . view _1)
    <*> (User.username . view _1)
    <*> (User.bio . view _1)
    <*> (User.image . view _1)
    <*> (fromMaybe False . view _2)

findProfile
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Maybe UserId
  -> Text
  -> m (Maybe Profile)
findProfile currentUserId username = do
  conn <- ask
  fmap toProfile
    <$> runSelect conn (select (selectProfile currentUserId username)) maybeRow
