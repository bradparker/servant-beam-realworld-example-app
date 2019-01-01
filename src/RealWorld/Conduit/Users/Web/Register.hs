module RealWorld.Conduit.Users.Web.Register
  ( Register
  , handler
  , Registrant(..)
  ) where

import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import RealWorld.Conduit.Environment (Environment(..))
import qualified RealWorld.Conduit.Users.Database as Database
import RealWorld.Conduit.Users.Web.Account (Account, fromUser)
import RealWorld.Conduit.Web.Errors (failedValidation, internalServerError)
import RealWorld.Conduit.Web.Namespace (Namespace(Namespace))
import Servant (Handler(Handler))
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
  Namespace <$> register environment params

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
  -> Handler Account
register Environment {withDatabaseConnection, jwtSettings} registrant =
  Handler $
  withDatabaseConnection $ \conn -> do
    attributes <-
      withExceptT failedValidation $
      usingReaderT conn $
      Database.attributesForInsert
        (password registrant)
        (email registrant)
        (username registrant)
        ""
        Nothing
    user <- withExceptT (internalServerError . show) $
      usingReaderT conn $
      Database.create attributes
    withExceptT (internalServerError . show) $
      fromUser jwtSettings user
