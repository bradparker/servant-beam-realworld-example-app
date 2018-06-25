module Main (main) where

import Control.Monad ((=<<))
import RealWorld.Conduit.Options (getOptions)
import System.IO (IO, print)

main :: IO ()
main = print =<< getOptions
