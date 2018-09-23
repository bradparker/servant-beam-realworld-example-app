{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module RealWorld.Conduit.Database
  ( ConduitDb(..)
  , conduitDb
  , openConduitDb
  , insertOne
  , findBy
  ) where

import Control.Applicative (pure)
import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe, listToMaybe, maybe)
import Database.Beam
  ( Beamable
  , Database
  , DatabaseEntity
  , DatabaseSettings
  , HasSqlEqualityCheck
  , Identity
  , Q
  , QExpr
  , SqlInsertValues
  , TableEntity
  , (==.)
  , defaultDbSettings
  , filter_
  , runSelectReturningOne
  , select
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax)
import Database.Beam.Backend.Types (Exposed, FromBackendRow)
import Database.Beam.Postgres (Postgres, runBeamPostgres)
import Database.Beam.Postgres.Syntax
  ( PgExpressionSyntax
  , PgInsertValuesSyntax
  , PgSelectSyntax
  , PgValueSyntax
  )
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import GHC.Generics (Generic)
import RealWorld.Conduit.Users.Database.Follow (FollowT)
import RealWorld.Conduit.Users.Database.User (UserT)
import System.IO (IO)
import Text.Show (Show)

data ConduitDb f = ConduitDb
  { conduitFollows :: f (TableEntity FollowT)
  , conduitUsers :: f (TableEntity UserT)
  } deriving (Generic)

instance Database Postgres ConduitDb

conduitDb :: DatabaseSettings Postgres ConduitDb
conduitDb = defaultDbSettings

openConduitDb :: MonadIO m => ByteString -> m Connection
openConduitDb = liftIO . connectPostgreSQL

data UnexpectedEmptyReturn =
  UnexpectedEmptyReturn
  deriving (Show)

instance Exception UnexpectedEmptyReturn

insertOne ::
     ( FromBackendRow Postgres (table Identity)
     , Generic (table Exposed)
     , Generic (table Identity)
     , Beamable table
     , MonadBeamInsertReturning syntax Postgres Connection m
     )
  => Connection
  -> DatabaseEntity Postgres ConduitDb (TableEntity table)
  -> SqlInsertValues PgInsertValuesSyntax (table (QExpr PgExpressionSyntax s))
  -> IO (table Identity)
insertOne conn table =
  maybe (throwIO UnexpectedEmptyReturn) pure <=<
  fmap listToMaybe . runBeamPostgres conn . runInsertReturningList table

findBy ::
     ( HasSqlValueSyntax PgValueSyntax a
     , HasSqlEqualityCheck PgExpressionSyntax a
     , FromBackendRow Postgres (table Identity)
     , Generic (table Exposed)
     , Generic (table Identity)
     , Beamable table
     )
  => Connection
  -> Q PgSelectSyntax ConduitDb _ (table (QExpr PgExpressionSyntax s))
  -> (table (QExpr PgExpressionSyntax s) -> QExpr PgExpressionSyntax _ a)
  -> a
  -> IO (Maybe (table Identity))
findBy conn scope column val =
  runBeamPostgres conn $
  runSelectReturningOne $ select $ filter_ ((val_ val ==.) . column) scope
