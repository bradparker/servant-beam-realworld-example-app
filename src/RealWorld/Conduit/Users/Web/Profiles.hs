module RealWorld.Conduit.Users.Web.Profiles
  ( Profiles
  , server
  ) where

import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Users.Web.Profiles.View (View)
import qualified RealWorld.Conduit.Users.Web.Profiles.View as View
import RealWorld.Conduit.Users.Web.Profiles.Follow (Follow)
import qualified RealWorld.Conduit.Users.Web.Profiles.Follow as Follow
import Servant (Server, (:<|>)((:<|>)))

type Profiles = View :<|> Follow

server :: Handle -> Server Profiles
server handle = View.handler handle :<|> Follow.handler handle
