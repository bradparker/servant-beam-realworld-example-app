module RealWorld.Conduit.Web
  ( app
  ) where

import Data.Function ((.))
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Web.API (API)
import qualified RealWorld.Conduit.Web.API as API
import RealWorld.Conduit.Web.Swagger (RealWorldConduitSwagger)
import qualified RealWorld.Conduit.Web.Swagger as Swagger
import Servant ((:<|>)((:<|>)), Server, serve)

type RealWorldConduit =
  RealWorldConduitSwagger :<|>
  API

server :: Handle -> Server RealWorldConduit
server handle =
  Swagger.server :<|>
  API.server handle

realWorldConduit :: Proxy RealWorldConduit
realWorldConduit = Proxy

app :: Handle -> Application
app = serve realWorldConduit . server
