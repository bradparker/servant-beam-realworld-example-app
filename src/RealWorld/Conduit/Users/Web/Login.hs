module RealWorld.Conduit.Users.Web.Login
  ( Login
  , handler
  ) where

import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((.))
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Pool (withResource)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.Credentials (Credentials)
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler, err401, errBody, throwError)
import Servant.API ((:>), JSON, Post, ReqBody)

type Login =
  "api" :>
  "users" :>
  "login" :>
  ReqBody '[JSON] (Namespace "user" Credentials) :>
  Post '[JSON] (Namespace "user" Account)

handler ::
     Handle
  -> Namespace "user" Credentials
  -> Handler (Namespace "user" Account)
handler Handle {connectionPool, authSecret} (Namespace params) = do
  found <-
    fmap (Namespace . fromUser authSecret) <$>
    liftIO (withResource connectionPool (`Database.findByCredentials` params))
  case found of
    Nothing -> throwError (err401 {errBody = "Incorrect login or password"})
    Just user -> pure user
