module RealWorld.Conduit.Web.Health
  ( server
  , Health
  ) where

import Control.Exception (catch)
import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import Data.Time (diffUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only, query_)
import RealWorld.Conduit.Environment (Environment(..))
import RealWorld.Conduit.Web.Health.Service (Service(Service))
import qualified RealWorld.Conduit.Web.Health.Service as Service
import Servant (Handler, Server)
import Servant.API ((:>), Get, JSON)

data Status = Status
  { title :: Text
  , status :: Bool
  , services :: [Service]
  } deriving (Generic)

deriving instance ToJSON Status
deriving instance ToSchema Status

type Health = "health" :> Get '[ JSON] Status

checkDatabase :: Environment -> IO Service
checkDatabase Environment {withDatabaseConnection} = do
  start <- getCurrentTime
  status <- withDatabaseConnection check `catch` failCheck
  end <- getCurrentTime
  pure
    Service
      { Service.title = "Database"
      , Service.status
      , Service.duration = diffUTCTime end start
      }
  where
    failCheck :: SomeException -> IO Bool
    failCheck = const (pure False)
    check :: Connection -> IO Bool
    check conn =
      True <$ (query_ conn "select 1" :: IO [Only Int])

health :: Environment -> Handler Status
health environment = do
  services <- liftIO $ sequence [checkDatabase environment]
  pure
    Status
      { title = "Realworld Conduit Api"
      , status = all Service.status services
      , services
      }

server :: Environment -> Server Health
server = health
