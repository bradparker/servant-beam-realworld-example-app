module RealWorld.Conduit.Web.API
  ( API
  , server
  ) where

import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Users.Web (Users)
import qualified RealWorld.Conduit.Users.Web as Users
import RealWorld.Conduit.Articles.Web (Articles)
import qualified RealWorld.Conduit.Articles.Web as Articles
import RealWorld.Conduit.Web.Health (Health)
import qualified RealWorld.Conduit.Web.Health as Health
import Servant (Server)
import Servant.API ((:<|>)((:<|>)))

type API = Health :<|> Users :<|> Articles

server :: Environment -> Server API
server environment =
  Health.server environment :<|>
  Users.server environment :<|>
  Articles.server environment
