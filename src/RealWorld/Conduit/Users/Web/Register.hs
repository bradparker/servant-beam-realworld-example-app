module RealWorld.Conduit.Users.Web.Register
  ( Register
  , handler
  , Error(..)
  , ValidationFailure(..)
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.Pool (withResource)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import RealWorld.Conduit.Handle (Handle(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User.Attributes (ValidationFailure)
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr, err422, errBody)
import Servant.API ((:>), JSON, PostCreated, ReqBody)
import System.IO (IO)
import Text.Show (Show)

type Register =
  "api" :>
  "users" :>
  ReqBody '[JSON] (Namespace "user" Registrant) :>
  PostCreated '[JSON] (Namespace "user" Account)

handler ::
     Handle
  -> Namespace "user" Registrant
  -> Handler (Namespace "user" Account)
handler handle (Namespace params) =
  Handler $
  withExceptT toServantError $
  Namespace <$> register handle params

newtype Error = FailedValidation
  { errors :: [ValidationFailure]
  } deriving (Generic)

deriving instance ToJSON Error

toServantError :: Error -> ServantErr
toServantError e@(FailedValidation _) = err422 {errBody = encode e}

data Registrant = Registrant
  { password :: Text
  , email :: Text
  , username :: Text
  } deriving (Generic, Show)

deriving instance FromJSON Registrant
deriving instance ToSchema Registrant

register ::
     Handle
  -> Registrant
  -> ExceptT Error IO Account
register Handle {connectionPool, authSecret} registrant =
  withResource connectionPool $ \conn -> do
    attributes <-
      withExceptT FailedValidation $
      Attributes.forInsert
        conn
        (password registrant)
        (email registrant)
        (username registrant)
        ""
        Nothing
    lift $ fromUser authSecret <$> Database.create conn attributes
