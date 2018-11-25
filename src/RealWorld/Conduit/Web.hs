module RealWorld.Conduit.Web
  ( app
  , context
  ) where

import Network.Wai (Application)
import RealWorld.Conduit.Environment (Environment(..))
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

server :: Environment -> Server RealWorldConduit
server handle =
  Swagger.server :<|>
  API.server handle

realWorldConduit :: Proxy RealWorldConduit
realWorldConduit = Proxy

context :: Environment -> Context '[CookieSettings, JWTSettings]
context handle =
  defaultCookieSettings :. jwtSettings handle :. EmptyContext

app :: Environment -> Application
app handle = serveWithContext realWorldConduit (context handle) (server handle)
