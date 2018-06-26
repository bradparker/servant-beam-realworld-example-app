module RealWorld.Conduit.Web.Health.Service
  ( Service(..)
  ) where

import Data.Aeson (ToJSON)
import Data.Bool (Bool)
import Data.Text (Text)
import GHC.Exts (Double)
import GHC.Generics (Generic)

data Service = Service
  { title :: Text
  , status :: Bool
  , duration :: Double
  } deriving (Generic)

deriving instance ToJSON Service
