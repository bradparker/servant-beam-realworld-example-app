module RealWorld.Conduit.Web.Health
  ( server
  , Health
  ) where

import Control.Applicative (pure)
import Data.Aeson (ToJSON)
import Data.Bool (Bool)
import Data.List (all)
import Data.Text (Text)
import GHC.Generics (Generic)
import RealWorld.Conduit.Web.Health.Service (Service)
import qualified RealWorld.Conduit.Web.Health.Service as Service
import Servant (Handler, Server)
import Servant.API ((:>), Get, JSON)

data Status = Status
  { revision :: Text
  , title :: Text
  , status :: Bool
  , services :: [Service]
  } deriving (Generic)

deriving instance ToJSON Status

type Health = "health" :> Get '[JSON] Status

health :: Handler Status
health = pure Status
  { revision = "HEAD"
  , title = "Realworld Conduit Api"
  , status = serviceStatus
  , services
  }
  where
    services = []
    serviceStatus = all Service.status services

server :: Server Health
server = health
