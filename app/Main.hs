module Main
  ( main
  ) where

import Data.Aeson (ToJSON, encode)
import qualified Data.CaseInsensitive as CI
import Data.Default (def)
import Data.Map (singleton)
import Network.Wai.Middleware.Cors (simpleCorsResourcePolicy, cors, corsRequestHeaders)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(CustomOutputFormatWithDetails)
  , RequestLoggerSettings(outputFormat)
  , mkRequestLogger
  )
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import qualified RealWorld.Conduit.Environment as Environment
import RealWorld.Conduit.Options (Options(port), getOptions)
import RealWorld.Conduit.Web (app)

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
  environment <- Environment.new options
  logger <- jsonLogger
  putStrLn (encode (startupEvent options))
  let resourcePolicy = simpleCorsResourcePolicy { corsRequestHeaders = [CI.mk "Content-Type" ] }
  run (port options) (logger . (cors . const . Just $ resourcePolicy ) $ (app environment))
