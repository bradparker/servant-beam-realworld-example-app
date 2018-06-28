module Main
  ( main
  ) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Default (def)
import Data.Map (Map, singleton)
import Data.String (String)
import GHC.Generics (Generic)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(CustomOutputFormatWithDetails)
  , RequestLoggerSettings(outputFormat)
  , mkRequestLogger
  )
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import RealWorld.Conduit.Options (Options(port), getOptions)
import RealWorld.Conduit.Web (app)
import System.IO (IO)
import Text.Show (show)

data Event a = Event
  { event :: String
  , payload :: a
  } deriving Generic

instance ToJSON a => ToJSON (Event a)

startupEvent :: Options -> Event (Map String String)
startupEvent options =
  Event {event = "startup", payload = singleton "port" (show (port options))}

jsonLogger :: IO Middleware
jsonLogger =
  mkRequestLogger
    def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

main :: IO ()
main = do
  options <- getOptions
  logger <- jsonLogger
  putStrLn (encode (startupEvent options))
  run (port options) (logger app)
