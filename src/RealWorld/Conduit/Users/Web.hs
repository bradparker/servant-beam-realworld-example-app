module RealWorld.Conduit.Users.Web
  ( Users
  , users
  , server
  ) where

import RealWorld.Conduit.Users.Web.Register (Register)
import qualified RealWorld.Conduit.Users.Web.Register as Register
import RealWorld.Conduit.Handle (Handle)
import Servant (Server)
import Data.Proxy (Proxy(Proxy))

type Users = Register

server :: Handle -> Server Users
server = Register.handler

users :: Proxy Users
users = Proxy
