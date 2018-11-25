module RealWorld.Conduit.Articles.Web.Article.Attributes
  ( Attributes(..)
  , Create
  , Update
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import RealWorld.Conduit.Attribute (Attribute)

data Attributes f = Attributes
  { title :: Attribute f Text
  , description :: Attribute f Text
  , body :: Attribute f Text
  , tagList :: Attribute f (Set Text)
  }

type Create = Attributes Identity

deriving instance Generic Create
deriving instance ToJSON Create
deriving instance FromJSON Create
deriving instance ToSchema Create

type Update = Attributes Maybe

deriving instance Generic Update
deriving instance ToJSON Update
deriving instance FromJSON Update
deriving instance ToSchema Update
