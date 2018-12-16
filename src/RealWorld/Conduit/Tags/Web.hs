module RealWorld.Conduit.Tags.Web
  ( server
  , Tags
  ) where

import Prelude hiding (All)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Tags.Database as Database
import RealWorld.Conduit.Tags.Database.Tag as Persisted
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), Server)
import Servant.API ((:>), Get, JSON)

type All =
  "api" :>
  "tags" :>
  Get '[JSON] (Namespace "tags" [Text])

query :: Environment -> Handler (Namespace "tags" [Text])
query environment =
  Handler $
  withDatabaseConnection environment $ \conn ->
    liftIO $ Namespace . map Persisted.name <$> Database.query conn

type Tags = All

server :: Environment -> Server Tags
server = query
