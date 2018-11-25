module RealWorld.Conduit.Articles.Web
  ( Articles
  , articles
  , server
  ) where

import Data.Proxy (Proxy(Proxy))
import RealWorld.Conduit.Articles.Web.Create (Create)
import qualified RealWorld.Conduit.Articles.Web.Create as Create
import RealWorld.Conduit.Articles.Web.Destroy (Destroy)
import qualified RealWorld.Conduit.Articles.Web.Destroy as Destroy
import RealWorld.Conduit.Articles.Web.Favorite (Favorite)
import qualified RealWorld.Conduit.Articles.Web.Favorite as Favorite
import RealWorld.Conduit.Articles.Web.Unfavorite (Unfavorite)
import qualified RealWorld.Conduit.Articles.Web.Unfavorite as Unfavorite
import RealWorld.Conduit.Articles.Web.Update (Update)
import qualified RealWorld.Conduit.Articles.Web.Update as Update
import RealWorld.Conduit.Articles.Web.View (View)
import qualified RealWorld.Conduit.Articles.Web.View as View
import RealWorld.Conduit.Articles.Web.All (All)
import qualified RealWorld.Conduit.Articles.Web.All as All
import RealWorld.Conduit.Handle (Handle)
import Servant ((:<|>)((:<|>)), Server)

type Articles =
  Create :<|>
  View :<|>
  Update :<|>
  Destroy :<|>
  Favorite :<|>
  Unfavorite :<|>
  All

server :: Handle -> Server Articles
server handle =
  Create.handler handle :<|>
  View.handler handle :<|>
  Update.handler handle :<|>
  Destroy.handler handle :<|>
  Favorite.handler handle :<|>
  Unfavorite.handler handle :<|>
  All.handler handle

articles :: Proxy Articles
articles = Proxy
