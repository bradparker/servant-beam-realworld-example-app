module RealWorld.Conduit.Web
  ( app
  , context
  ) where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Web.API (API)
import qualified RealWorld.Conduit.Web.API as API
import RealWorld.Conduit.Web.Swagger (RealWorldConduitSwagger)
import qualified RealWorld.Conduit.Web.Swagger as Swagger
import Servant
  ( (:<|>)((:<|>))
  , Context((:.), EmptyContext)
  , Server
  , serveWithContext
  )
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings)

type RealWorldConduit =
  RealWorldConduitSwagger :<|>
  API

server :: Handle -> Server RealWorldConduit
server handle =
  Swagger.server :<|>
  API.server handle

realWorldConduit :: Proxy RealWorldConduit
realWorldConduit = Proxy

context :: Handle -> Context '[CookieSettings, JWTSettings]
context handle =
  defaultCookieSettings :. jwtSettings handle :. EmptyContext

app :: Handle -> Application
app handle = serveWithContext realWorldConduit (context handle) (server handle)
