module RealWorld.Conduit.Users.Web.Register
  ( Register
  , handler
  , Error(..)
  , ValidationFailure(..)
  , Registrant(..)
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Swagger (ToSchema)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Database.User.Attributes (ValidationFailure)
import qualified RealWorld.Conduit.Users.Database.User.Attributes as Attributes
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler), ServantErr, err422, err500, errBody)
import Servant.API ((:>), JSON, PostCreated, ReqBody)

type Register =
  "api" :>
  "users" :>
  ReqBody '[JSON] (Namespace "user" Registrant) :>
  PostCreated '[JSON] (Namespace "user" Account)

handler ::
     Environment
  -> Namespace "user" Registrant
  -> Handler (Namespace "user" Account)
handler environment (Namespace params) =
  Handler $
  withExceptT toServantError $
  Namespace <$> register environment params

data Error
  = FailedValidation [ValidationFailure]
  | InternalServerError Text

data ErrorPayload e = ErrorPayload
  { message :: Text
  , errors :: e
  } deriving (Generic)

deriving instance ToJSON e => ToJSON (ErrorPayload e)

toServantError :: Error -> ServantErr
toServantError (FailedValidation e) =
  err422 {errBody = encode (ErrorPayload "Failed validation" e)}
toServantError (InternalServerError e) =
  err500 {errBody = encode (ErrorPayload "Internal server error" e)}

data Registrant = Registrant
  { password :: Text
  , email :: Text
  , username :: Text
  } deriving (Generic, Show)

deriving instance ToJSON Registrant
deriving instance FromJSON Registrant
deriving instance ToSchema Registrant

register ::
     Environment
  -> Registrant
  -> ExceptT Error IO Account
register Environment {withDatabaseConnection, jwtSettings} registrant =
  withDatabaseConnection $ \conn -> do
    attributes <-
      withExceptT FailedValidation $
      Attributes.forInsert
        conn
        (password registrant)
        (email registrant)
        (username registrant)
        ""
        Nothing
    user <- lift $ Database.create conn attributes
    withExceptT (InternalServerError . show) $ fromUser jwtSettings user
