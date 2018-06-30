module RealWorld.Conduit.Web.API
  ( API
  , server
  ) where

import RealWorld.Conduit.Web.Health (Health)
import Servant (Server)
import RealWorld.Conduit.Handle (Handle)
import qualified RealWorld.Conduit.Web.Health as Health

type API = Health

server :: Handle -> Server API
server = Health.server
