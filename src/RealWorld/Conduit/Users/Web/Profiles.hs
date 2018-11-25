module RealWorld.Conduit.Users.Web.Profiles
  ( Profiles
  , server
  ) where

import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Users.Web.Profiles.Follow (Follow)
import qualified RealWorld.Conduit.Users.Web.Profiles.Follow as Follow
import RealWorld.Conduit.Users.Web.Profiles.Unfollow (Unfollow)
import qualified RealWorld.Conduit.Users.Web.Profiles.Unfollow as Unfollow
import RealWorld.Conduit.Users.Web.Profiles.View (View)
import qualified RealWorld.Conduit.Users.Web.Profiles.View as View
import Servant ((:<|>)((:<|>)), Server)

type Profiles = View :<|> Follow :<|> Unfollow

server :: Environment -> Server Profiles
server environment =
  View.handler environment :<|>
  Follow.handler environment :<|>
  Unfollow.handler environment
