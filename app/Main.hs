module Main
  ( main
  ) where

import Data.Function (($), (&))
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setLogger
  , setPort
  )
import Network.Wai.Logger (withStdoutLogger)
import RealWorld.Conduit.Options (Options(port), getOptions)
import RealWorld.Conduit.Web (app)
import System.IO (IO, putStrLn)
import Text.Show (show)
import Data.Semigroup ((<>))

main :: IO ()
main = do
  options <- getOptions
  withStdoutLogger $ \logger -> do
    let settings = defaultSettings & setPort (port options) & setLogger logger
    putStrLn ("Server starting on: " <> show (port options))
    runSettings settings app
