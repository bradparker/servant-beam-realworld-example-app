module RealWorld.Conduit.Web.Swagger
  ( server
  , RealWorldConduitSwagger
  ) where

import Control.Lens ((.~), (?~))
import Data.Function ((&), (.))
import Data.Proxy (Proxy(Proxy))
import Data.Swagger (Swagger, description, info, title, version)
import RealWorld.Conduit.Web.API (API)
import Servant (Server)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type RealWorldConduitSwagger = SwaggerSchemaUI "swagger" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    & info . title .~ "RealWorld Conduit API"
    & info . version .~ "0.1.0.0"
    & info . description ?~ "Exemplary fullstack Medium.com clone powered by Servant and Beam"

server :: Server RealWorldConduitSwagger
server = swaggerSchemaUIServer swaggerDoc
