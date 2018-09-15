module RealWorld.Conduit.Web.Errors
  ( ErrorBody(..)
  , notFound
  , failedValidation
  , internalServerError
  ) where

import Data.Text (Text)
import Data.Maybe (Maybe(Nothing, Just))
import Servant (err404, err422, err500, errBody, ServantErr)
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)
import Data.Semigroup ((<>))

data ErrorBody errors = ErrorBody
  { message :: Text
  , errors :: Maybe errors
  } deriving (Generic)

deriving instance ToJSON errors => ToJSON (ErrorBody errors)

notFound :: Text -> ServantErr
notFound resourceName =
  err404 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = resourceName <> " not found"
        , errors = Nothing
        }

failedValidation :: ToJSON failures => failures -> ServantErr
failedValidation failures =
  err422 {errBody = encode (body failures)}
    where
      body :: ToJSON failures => failures -> ErrorBody failures
      body fs = ErrorBody
        { message = "Failed validation"
        , errors = Just fs
        }

internalServerError :: Text -> ServantErr
internalServerError message =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [message]
        }
