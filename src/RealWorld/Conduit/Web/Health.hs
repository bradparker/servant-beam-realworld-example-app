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
import Data.Functor (void)
import Data.Int (Int)
import Data.List (all)
import Data.Pool (Pool, withResource)
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
  { revision :: Text
  , title :: Text
  , status :: Bool
  , services :: [Service]
  } deriving (Generic)

deriving instance ToJSON Status

type Health = "health" :> Get '[ JSON] Status

checkDatabase :: Handle -> IO Service
checkDatabase Handle {connectionPool} = do
  start <- getCurrentTime
  status <- check connectionPool `catch` failCheck
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
    check :: Pool Connection -> IO Bool
    check pool = do
      void (withResource pool (`query_` "select 1") :: IO [Only Int])
      pure True

health :: Handle -> Handler Status
health handle = do
  services <- liftIO $ sequenceA [checkDatabase handle]
  pure
    Status
      { revision = "HEAD"
      , title = "Realworld Conduit Api"
      , status = all Service.status services
      , services
      }

server :: Handle -> Server Health
server = health
