module RealWorld.Conduit.Users.Web.Current
  ( Current
  , server
  ) where

import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Users.Web.Current.View (View)
import qualified RealWorld.Conduit.Users.Web.Current.View as View
import RealWorld.Conduit.Users.Web.Current.Update (Update)
import qualified RealWorld.Conduit.Users.Web.Current.Update as Update
import Servant ((:<|>)((:<|>)), Server)

type Current = View :<|> Update

server :: Environment -> Server Current
server environment =
  View.handler environment :<|>
  Update.handler environment
