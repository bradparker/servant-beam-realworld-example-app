module RealWorld.Conduit.Users.Web.Claim
  ( Claim(..)
  , fromUser
  , deriveToken
  ) where

import Crypto.JOSE (Error)
import Data.Aeson (FromJSON, ToJSON)
import Database.Beam (primaryKey)
import RealWorld.Conduit.Users.Database.User (PrimaryKey(unUserId), User)
import Servant.Auth.Server (FromJWT, JWTSettings, ToJWT)
import qualified Servant.Auth.Server as ServantAuth
import Data.Swagger (ToSchema)

newtype Claim =
  Claim { id :: Int }
  deriving (Generic)

instance ToJSON Claim
instance FromJSON Claim
instance ToJWT Claim
instance FromJWT Claim
deriving instance ToSchema Claim

fromUser :: User -> Claim
fromUser =
  Claim . unUserId . primaryKey

deriveToken :: JWTSettings -> User -> ExceptT Error IO Text
deriveToken settings user =
  toStrict . decodeUtf8 <$> ExceptT (ServantAuth.makeJWT (fromUser user) settings Nothing)
