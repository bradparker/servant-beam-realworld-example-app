module Main
  ( main
  ) where

import Data.Default (def)
import Data.Semigroup ((<>))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(CustomOutputFormatWithDetails)
  , RequestLoggerSettings(outputFormat)
  , mkRequestLogger
  )
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import RealWorld.Conduit.Options (Options(port), getOptions)
import RealWorld.Conduit.Web (app)
import System.IO (IO, putStrLn)
import Text.Show (show)

main :: IO ()
main = do
  options <- getOptions
  loggerMiddleware <-
    mkRequestLogger
      def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
  putStrLn ("Server starting on: " <> show (port options))
  run (port options) (loggerMiddleware app)
