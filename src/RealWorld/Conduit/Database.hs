{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module RealWorld.Conduit.Database
  ( ConduitDb(..)
  , conduitDb
  , openConduitDb
  , insertOne
  , findBy
  ) where

import Control.Exception (throwIO)
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
  , dbModification
  , defaultDbSettings
  , fieldNamed
  , filter_
  , modifyTable
  , runSelectReturningOne
  , select
  , tableModification
  , val_
  , withDbModification
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
import RealWorld.Conduit.Articles.Database.Article (ArticleT)
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import RealWorld.Conduit.Articles.Database.ArticleTag (ArticleTagT)
import RealWorld.Conduit.Articles.Database.Favorite (FavoriteT)
import RealWorld.Conduit.Tags.Database.Tag (TagT)
import RealWorld.Conduit.Users.Database.Follow (FollowT)
import RealWorld.Conduit.Users.Database.User (UserT)

data ConduitDb f = ConduitDb
  { conduitArticleTags :: f (TableEntity ArticleTagT)
  , conduitArticles :: f (TableEntity ArticleT)
  , conduitFavorites :: f (TableEntity FavoriteT)
  , conduitFollows :: f (TableEntity FollowT)
  , conduitTags :: f (TableEntity TagT)
  , conduitUsers :: f (TableEntity UserT)
  } deriving (Generic)

instance Database Postgres ConduitDb

conduitDb :: DatabaseSettings Postgres ConduitDb
conduitDb =
  defaultDbSettings `withDbModification`
  dbModification
    { conduitArticles =
        modifyTable id $
        tableModification
          { Article.createdAt = fieldNamed "created_at"
          , Article.updatedAt = fieldNamed "updated_at"
          }
    }

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
