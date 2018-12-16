module RealWorld.Conduit.Web.API
  ( API
  , server
  ) where

import RealWorld.Conduit.Articles.Web (Articles)
import qualified RealWorld.Conduit.Articles.Web as Articles
import RealWorld.Conduit.Comments.Web (Comments)
import qualified RealWorld.Conduit.Comments.Web as Comments
import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Tags.Web (Tags)
import qualified RealWorld.Conduit.Tags.Web as Tags
import RealWorld.Conduit.Users.Web (Users)
import qualified RealWorld.Conduit.Users.Web as Users
import RealWorld.Conduit.Web.Health (Health)
import qualified RealWorld.Conduit.Web.Health as Health
import Servant (Server)
import Servant.API ((:<|>)((:<|>)))

type API = Health :<|> Users :<|> Articles :<|> Comments :<|> Tags

server :: Environment -> Server API
server environment =
  Health.server environment :<|>
  Users.server environment :<|>
  Articles.server environment :<|>
  Comments.server environment :<|>
  Tags.server environment
