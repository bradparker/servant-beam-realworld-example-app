module RealWorld.Conduit.Web.Health.Service
  ( Service(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Bool (Bool)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

data Service = Service
  { title :: Text
  , status :: Bool
  , duration :: NominalDiffTime
  } deriving (Generic)

deriving instance ToJSON Service
deriving instance ToSchema Service
