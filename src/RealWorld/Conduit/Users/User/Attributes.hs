module RealWorld.Conduit.Users.User.Attributes
  ( Attributes(..)
  ) where

import RealWorld.Conduit.Attribute (Attribute)

data Attributes f = Attributes
  { password :: Attribute f Text
  , email :: Attribute f Text
  , username :: Attribute f Text
  , bio :: Attribute f Text
  , image :: Attribute f (Maybe Text)
  }
