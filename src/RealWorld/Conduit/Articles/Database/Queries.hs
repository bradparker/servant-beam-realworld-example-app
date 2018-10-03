{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module RealWorld.Conduit.Articles.Database.Queries
  ( decorate
  , findBySlug
  , findByTitle
  ) where

import Control.Applicative ((<*>), pure)
import Control.Lens (_1, _2, _3, _4, _5, view)
import Data.Bool (Bool(False))
import Data.Foldable (foldr1)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List.NonEmpty (groupWith)
import Data.Maybe (Maybe, listToMaybe, maybe, maybeToList)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Tuple (fst, snd)
import Database.Beam
  ( ManyToMany
  , Nullable
  , Q
  , QExpr
  , (==.)
  , aggregate_
  , all_
  , count_
  , filter_
  , group_
  , just_
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
import RealWorld.Conduit.Articles.Database.Article (Article, ArticleT(..))
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import qualified RealWorld.Conduit.Articles.Database.ArticleTag as ArticleTag
import RealWorld.Conduit.Articles.Database.Decorated (Decorated(Decorated))
import qualified RealWorld.Conduit.Articles.Database.Decorated as Decorated
import RealWorld.Conduit.Articles.Database.Favorite (FavoriteT(..))
import qualified RealWorld.Conduit.Articles.Database.Favorite as Favorite
import RealWorld.Conduit.Database (ConduitDb(..), conduitDb, findBy)
import RealWorld.Conduit.Tags.Database.Tag (TagT(..))
import qualified RealWorld.Conduit.Tags.Database.Tag as Tag
import RealWorld.Conduit.Users.Database.User (PrimaryKey(unUserId), User, UserT)
import System.IO (IO)

findByTitle :: Connection -> Text -> IO (Maybe Article)
findByTitle conn = findBy conn (all_ (conduitArticles conduitDb)) Article.title

findBySlug :: Connection -> Text -> IO (Maybe Article)
findBySlug conn = findBy conn (all_ (conduitArticles conduitDb)) Article.slug

decorate :: Connection -> Maybe User -> Article -> IO (Maybe Decorated)
decorate conn currentUser article =
  listToMaybe <$>
  findDecorated
    conn
    currentUser
    (filter_
       ((val_ (primaryKey article) ==.) . primaryKey)
       (all_ (conduitArticles conduitDb)))

findDecorated ::
     Connection
  -> Maybe User
  -> Q PgSelectSyntax ConduitDb _ (ArticleT (QueryExpression _))
  -> IO [Decorated]
findDecorated conn currentUser scope =
  aggregateRows <$>
  runBeamPostgres
    conn
    (runSelectReturningList $
     select $
     aggregate_
       (\(article, author, tag, fav, currentUserFavorited) ->
          ( group_ article
          , group_ author
          , group_ (Tag.name tag)
          , count_ (unUserId (Favorite.user fav))
          , group_ currentUserFavorited)) $ do
       article <- scope
       author <- authors article
       tag <- tags article
       fav <- favorites article
       pure
         ( article
         , author
         , tag
         , fav
         , maybe
             (val_ False)
             ((Favorite.user fav ==.) . just_ . val_ . primaryKey)
             currentUser))

aggregateRows :: [(Article, User, Maybe Text, Int, Bool)] -> [Decorated]
aggregateRows =
  (foldr1 (<>) <$>) . groupWith (Article.id . Decorated.article) . (toDecorated <$>)
  where
    toDecorated =
      Decorated
        <$> view _1
        <*> view _2
        <*> maybeToList . view _3
        <*> view _4
        <*> view _5

type QueryExpression s = QExpr PgExpressionSyntax s

authors ::
     ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (UserT (QueryExpression s))
authors Article {author} =
  filter_ ((author ==.) . primaryKey) (all_ (conduitUsers conduitDb))

articletagRelationship :: ManyToMany ConduitDb ArticleT TagT
articletagRelationship =
  manyToMany_ (conduitArticleTags conduitDb) ArticleTag.article ArticleTag.tag

articlesAndTags ::
     Q PgSelectSyntax ConduitDb s ( ArticleT (QExpr PgExpressionSyntax s)
                                  , TagT (QExpr PgExpressionSyntax s))
articlesAndTags =
  articletagRelationship
    (all_ (conduitArticles conduitDb))
    (all_ (conduitTags conduitDb))

tags ::
     ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (TagT (Nullable (QueryExpression s)))
tags article =
  snd <$>
  leftJoin_ articlesAndTags ((primaryKey article ==.) . primaryKey . fst)

favorites ::
     ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (FavoriteT (Nullable (QueryExpression s)))
favorites article =
  leftJoin_
    (all_ (conduitFavorites conduitDb))
    ((`references_` article) . Favorite.article)
