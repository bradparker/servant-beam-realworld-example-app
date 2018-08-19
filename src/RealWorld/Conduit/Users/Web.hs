module RealWorld.Conduit.Users.Web
  ( Users
  , users
  , server
  ) where

import Data.Proxy (Proxy(Proxy))
import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Users.Web.Login (Login)
import qualified RealWorld.Conduit.Users.Web.Login as Login
import RealWorld.Conduit.Users.Web.Register (Register)
import qualified RealWorld.Conduit.Users.Web.Register as Register
import Servant ((:<|>)((:<|>)), Server)

type Users = Register :<|> Login

server :: Handle -> Server Users
server handle = Register.handler handle :<|> Login.handler handle

users :: Proxy Users
users = Proxy
