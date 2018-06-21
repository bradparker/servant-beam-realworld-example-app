module RealWorld.Conduit.Attribute (Attribute) where

import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe)

type family Attribute f a where
  Attribute Identity a = a
  Attribute Maybe a = Maybe a
