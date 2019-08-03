module RealWorld.Conduit.Articles.Web
  ( Articles
  , articles
  , server
  ) where

import Data.Proxy (Proxy(Proxy))
import Prelude hiding (All)
import RealWorld.Conduit.Articles.Web.All (All)
import qualified RealWorld.Conduit.Articles.Web.All as All
import RealWorld.Conduit.Articles.Web.Create (Create)
import qualified RealWorld.Conduit.Articles.Web.Create as Create
import RealWorld.Conduit.Articles.Web.Destroy (Destroy)
import qualified RealWorld.Conduit.Articles.Web.Destroy as Destroy
import RealWorld.Conduit.Articles.Web.Favorite (Favorite)
import qualified RealWorld.Conduit.Articles.Web.Favorite as Favorite
import RealWorld.Conduit.Articles.Web.Feed (Feed)
import qualified RealWorld.Conduit.Articles.Web.Feed as Feed
import RealWorld.Conduit.Articles.Web.Unfavorite (Unfavorite)
import qualified RealWorld.Conduit.Articles.Web.Unfavorite as Unfavorite
import RealWorld.Conduit.Articles.Web.Update (Update)
import qualified RealWorld.Conduit.Articles.Web.Update as Update
import RealWorld.Conduit.Articles.Web.View (View)
import qualified RealWorld.Conduit.Articles.Web.View as View
import RealWorld.Conduit.Environment (Environment)
import Servant ((:>), (:<|>)((:<|>)), Server)

type Articles
  = "api" :>
    "articles" :>
      (    All
      :<|> Feed
      :<|> Create
      :<|> View
      :<|> Update
      :<|> Destroy
      :<|> Favorite
      :<|> Unfavorite
      )

server :: Environment -> Server Articles
server environment =
  All.handler environment
    :<|> Feed.handler environment
    :<|> Create.handler environment
    :<|> View.handler environment
    :<|> Update.handler environment
    :<|> Destroy.handler environment
    :<|> Favorite.handler environment
    :<|> Unfavorite.handler environment

articles :: Proxy Articles
articles = Proxy
