module RealWorld.Conduit.Web.Auth
  ( withRequiredAuth
  , withOptionalAuth
  ) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Text (Text)
import Servant (Handler, err401, errBody, throwError)
import Servant.Auth.Server (AuthResult(..))

notAuthorizedError :: Map.Map Text Text
notAuthorizedError = Map.singleton "message" "Not Authorized"

withRequiredAuth :: (a -> Handler b) -> AuthResult a -> Handler b
withRequiredAuth h (Authenticated a) = h a
withRequiredAuth _ _ = throwError err401 {errBody = encode notAuthorizedError}

withOptionalAuth :: (Maybe a -> Handler b) -> AuthResult a -> Handler b
withOptionalAuth h (Authenticated a) = h (Just a)
withOptionalAuth h _ = h Nothing
