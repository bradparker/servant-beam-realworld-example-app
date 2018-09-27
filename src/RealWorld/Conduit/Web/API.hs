module RealWorld.Conduit.Web.API
  ( API
  , server
  ) where

import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Users.Web (Users)
import qualified RealWorld.Conduit.Users.Web as Users
import RealWorld.Conduit.Articles.Web (Articles)
import qualified RealWorld.Conduit.Articles.Web as Articles
import RealWorld.Conduit.Web.Health (Health)
import qualified RealWorld.Conduit.Web.Health as Health
import Servant (Server)
import Servant.API ((:<|>)((:<|>)))

type API = Health :<|> Users :<|> Articles

server :: Handle -> Server API
server handle =
  Health.server handle :<|>
  Users.server handle :<|>
  Articles.server handle
