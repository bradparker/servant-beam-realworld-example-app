module RealWorld.Conduit.Web.Health
  ( server
  , Health
  ) where

import Control.Applicative (pure)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Bool (Bool(False, True))
import Data.Function (($), const)
import Data.Functor ((<$))
import Data.Int (Int)
import Data.List (all)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Traversable (sequenceA)
import Database.PostgreSQL.Simple (Connection, Only, query_)
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import RealWorld.Conduit.Web.Health.Service (Service(Service))
import qualified RealWorld.Conduit.Web.Health.Service as Service
import Servant (Handler, Server)
import Servant.API ((:>), Get, JSON)
import System.IO (IO)

data Status = Status
  { title :: Text
  , status :: Bool
  , services :: [Service]
  } deriving (Generic)

deriving instance ToJSON Status
deriving instance ToSchema Status

type Health = "health" :> Get '[ JSON] Status

checkDatabase :: Handle -> IO Service
checkDatabase Handle {withDatabaseConnection} = do
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

health :: Handle -> Handler Status
health handle = do
  services <- liftIO $ sequenceA [checkDatabase handle]
  pure
    Status
      { title = "Realworld Conduit Api"
      , status = all Service.status services
      , services
      }

server :: Handle -> Server Health
server = health
