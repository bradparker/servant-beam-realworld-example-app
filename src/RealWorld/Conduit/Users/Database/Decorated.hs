module RealWorld.Conduit.Users.Database.Decorated
  ( Decorated(..)
  ) where

import RealWorld.Conduit.Users.Database.User (User)

data Decorated = Decorated
  { user :: User
  , following :: Bool
  } deriving (Show, Eq, Ord)
