module RealWorld.Conduit.Web.Errors
  ( ErrorBody(..)
  , failedValidation
  , forbidden
  , internalServerError
  , notAuthorized
  , notFound
  ) where

import Servant (err401, err403, err404, err422, err500, errBody, ServantErr)
import Data.Aeson (ToJSON, encode)

data ErrorBody errors = ErrorBody
  { message :: Text
  , errors :: Maybe errors
  } deriving (Generic)

deriving instance ToJSON errors => ToJSON (ErrorBody errors)

notAuthorized :: ServantErr
notAuthorized =
  err401 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Not Authorized"
        , errors = Nothing
        }

forbidden :: ServantErr
forbidden =
  err403 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Forbidden"
        , errors = Nothing
        }

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
