{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module RealWorld.Conduit.Articles.Database.Queries
  ( QueryParams(..)
  , query
  , decorate
  , feed
  , findBySlug
  , findByTitle
  ) where

import Control.Lens (_1, _2, _3, _4, _5, _6, view)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database.Beam
  ( ManyToMany
  , Nullable
  , Q
  , QExpr
  , (&&.)
  , (==.)
  , aggregate_
  , all_
  , count_
  , desc_
  , filter_
  , filter_
  , group_
  , group_
  , guard_
  , in_
  , just_
  , just_
  , leftJoin_
  , leftJoin_
  , limit_
  , manyToMany_
  , manyToMany_
  , offset_
  , orderBy_
  , primaryKey
  , references_
  , runSelectReturningList
  , select
  , val_
  )
import Database.Beam.Postgres (pgArrayAgg, pgBoolOr, runBeamPostgres)
import Database.Beam.Postgres.Syntax (PgExpressionSyntax, PgSelectSyntax)
import Database.PostgreSQL.Simple (Connection)
import RealWorld.Conduit.Articles.Database.Article (Article, ArticleT(Article))
import qualified RealWorld.Conduit.Articles.Database.Article as Article
import qualified RealWorld.Conduit.Articles.Database.ArticleTag as ArticleTag
import RealWorld.Conduit.Articles.Database.Decorated (Decorated(Decorated))
import RealWorld.Conduit.Articles.Database.Favorite (FavoriteT(..))
import qualified RealWorld.Conduit.Articles.Database.Favorite as Favorite
import RealWorld.Conduit.Database (ConduitDb(..), conduitDb, findBy)
import RealWorld.Conduit.Tags.Database.Tag (TagT(..))
import qualified RealWorld.Conduit.Tags.Database.Tag as Tag
import RealWorld.Conduit.Users.Database (followersAndFollowees)
import qualified RealWorld.Conduit.Users.Database.Decorated as User
import qualified RealWorld.Conduit.Users.Database.Follow as Follow
import qualified RealWorld.Conduit.Users.Database.Queries as Users
import RealWorld.Conduit.Users.Database.User
  ( PrimaryKey(unUserId)
  , User
  , UserT(username)
  )

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
  (rowToDecorated <$>) <$>
  runBeamPostgres
    conn
    (runSelectReturningList $
     select $
     aggregate_
       (\(article, author, tag, fav, currentUserFollowing, currentUserFavorited) ->
          ( group_ article
          , group_ author
          , pgArrayAgg (Tag.name tag)
          , count_ (unUserId (Favorite.user fav))
          , pgBoolOr currentUserFollowing
          , pgBoolOr currentUserFavorited)) $ do
       article <- scope
       author <- authors article
       follow <- Users.follows author
       tag <- tags article
       fav <- favorites article
       pure
         ( article
         , author
         , tag
         , fav
         , maybe
             (val_ False)
             ((Follow.follower follow ==.) . just_ . val_ . primaryKey)
             currentUser
         , maybe
             (val_ False)
             ((Favorite.user fav ==.) . just_ . val_ . primaryKey)
             currentUser))

rowToDecorated ::
     (Article, User, Vector (Maybe Text), Int, Maybe Bool, Maybe Bool) -> Decorated
rowToDecorated =
  Decorated
    <$> view _1
    <*> (User.Decorated <$> view _2 <*> fromMaybe False . view _5)
    <*> Set.fromList . Vector.toList . Vector.mapMaybe id . view _3
    <*> view _4
    <*> fromMaybe False . view _6

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

data QueryParams = QueryParams
  { qpLimit :: Integer
  , qpOffset :: Integer
  , qpTags :: [Text]
  , qpAuthors :: [Text]
  , qpFavorited :: [Text]
  }

byAuthors ::
     [Text]
  -> ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
byAuthors usernames article = do
  unless (null usernames) $ do
    author <- authors article
    guard_ (username author `in_` map val_ usernames)
  pure article

taggedWith ::
     [Text]
  -> ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
taggedWith tagNames article = do
  unless (null tagNames) $ do
    tag <- tags article
    guard_ (Tag.name tag `in_` map (just_ . val_) tagNames)
  pure article

favoritedBy ::
     [Text]
  -> ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
favoritedBy usernames article = do
  unless (null usernames) $ do
    fav <- favorites article
    user <- all_ (conduitUsers conduitDb)
    guard_
      (username user `in_` map val_ usernames &&. just_ (primaryKey user) ==.
       Favorite.user fav)
  pure article

allMatching ::
     QueryParams -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
allMatching (QueryParams limit offset tagNames authorNames usersFavorited) =
  orderBy_ (desc_ . Article.createdAt) $
  limit_ limit $
  offset_ offset $ do
    article <- all_ (conduitArticles conduitDb)
    void $ favoritedBy usersFavorited article
    void $ taggedWith tagNames article
    void $ byAuthors authorNames article
    pure article

query ::
     Connection
  -> Maybe User
  -> QueryParams
  -> IO [Decorated]
query conn currentUser =
  findDecorated conn currentUser . allMatching

byFollowing ::
     User
  -> Integer
  -> Integer
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
byFollowing user limit offset = do
  article <- allMatching (QueryParams limit offset [] [] [])
  following <-
    snd <$>
    filter_
      ((val_ (primaryKey user) ==.) . primaryKey . fst)
      followersAndFollowees
  guard_ (Article.author article ==. primaryKey following)
  pure article

feed :: Connection -> User -> Integer -> Integer -> IO [Decorated]
feed conn user limit offset =
  findDecorated conn (Just user) (byFollowing user limit offset)
