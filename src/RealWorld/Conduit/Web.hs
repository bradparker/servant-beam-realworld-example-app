module RealWorld.Conduit.Web
  ( app
  ) where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import RealWorld.Conduit.Web.Health (Health)
import qualified RealWorld.Conduit.Web.Health as Health
import Servant (Server, serve)

type API = Health

api :: Proxy API
api = Proxy

server :: Server API
server = Health.server

app :: Application
app = serve api server
