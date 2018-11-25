module RealWorld.Conduit.Attribute (Attribute) where

type family Attribute f a where
  Attribute Identity a = a
  Attribute Maybe a = Maybe a
