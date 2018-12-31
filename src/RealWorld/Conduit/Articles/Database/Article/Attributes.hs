module RealWorld.Conduit.Articles.Database.Article.Attributes
  ( Attributes(..)
  ) where

import RealWorld.Conduit.Attribute (Attribute)

data Attributes f = Attributes
  { slug :: Attribute f Text
  , title :: Attribute f Text
  , description :: Attribute f Text
  , body :: Attribute f Text
  , tagList :: Attribute f (Set Text)
  }
