module RealWorld.Conduit.Users.Web
  ( Users
  , users
  , server
  ) where

import Data.Proxy (Proxy(Proxy))
import RealWorld.Conduit.Environment (Environment)
import RealWorld.Conduit.Users.Web.Current (Current)
import qualified RealWorld.Conduit.Users.Web.Current as Current
import RealWorld.Conduit.Users.Web.Profiles (Profiles)
import qualified RealWorld.Conduit.Users.Web.Profiles as Profiles
import RealWorld.Conduit.Users.Web.Login (Login)
import qualified RealWorld.Conduit.Users.Web.Login as Login
import RealWorld.Conduit.Users.Web.Register (Register)
import qualified RealWorld.Conduit.Users.Web.Register as Register
import Servant ((:<|>)((:<|>)), Server)

type Users =
  Register :<|>
  Login :<|>
  Current :<|>
  Profiles

server :: Environment -> Server Users
server environment =
  Register.handler environment :<|>
  Login.handler environment :<|>
  Current.server environment :<|>
  Profiles.server environment

users :: Proxy Users
users = Proxy
