module RealWorld.Conduit.Users.Web.Profiles
  ( Profiles
  , server
  ) where

import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Users.Web.Profiles.View (View)
import qualified RealWorld.Conduit.Users.Web.Profiles.View as View
import Servant (Server)

type Profiles = View

server :: Handle -> Server Profiles
server = View.handler
