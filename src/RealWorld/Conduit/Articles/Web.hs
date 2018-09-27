module RealWorld.Conduit.Articles.Web
  ( Articles
  , articles
  , server
  ) where

import Data.Proxy (Proxy(Proxy))
import RealWorld.Conduit.Handle (Handle)
import RealWorld.Conduit.Articles.Web.Create (Create)
import qualified RealWorld.Conduit.Articles.Web.Create as Create
import Servant (Server)

type Articles = Create

server :: Handle -> Server Articles
server = Create.handler

articles :: Proxy Articles
articles = Proxy
