module RealWorld.Conduit.Options
  ( Options(..)
  , ServerOptions(..)
  , DatabaseOptions(..)
  , getOptions
  ) where

import Control.Applicative ((<*>))
import Control.Monad ((=<<))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Function ((&), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (String)
import Options.Applicative
  ( Parser
  , auto
  , customExecParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , prefs
  , short
  , showHelpOnError
  , str
  , value
  )
import System.Environment (lookupEnv)
import System.IO (IO)
import Text.Read (readMaybe)
import Text.Show (Show)

defaults :: Options
defaults =
  Options
    { server = ServerOptions {port = 8080}
    , database = DatabaseOptions {url = "postgres://localhost:5432/conduit"}
    }

newtype ServerOptions = ServerOptions
  { port :: Int
  } deriving (Show)

newtype DatabaseOptions = DatabaseOptions
  { url :: ByteString
  } deriving (Show)

data Options = Options
  { server :: ServerOptions
  , database :: DatabaseOptions
  } deriving (Show)

optionsParser :: Int -> String -> Parser Options
optionsParser defaultPort defaultDatabaseUrl =
  Options <$>
    (ServerOptions <$>
     option
       auto
       (value defaultPort  <> short 'p' <> long "port" <>
        help "A port for the server to listen on" <>
        metavar "PORT")) <*>
    (DatabaseOptions . pack <$>
     option
       str
       (value defaultDatabaseUrl <> short 'd' <> long "database-url" <>
        help
          "A database url of the form postgres://user:password@hostname:port/name" <>
        metavar "DATABASE_URL"))

getOptions :: IO Options
getOptions = do
  port <- getPort
  databaseUrl <- getDatabaseUrl
  customExecParser
    (prefs showHelpOnError)
    (info
       (helper <*> optionsParser port databaseUrl)
       (header "Realworld Conduit" <> fullDesc))
  where
    getPort =
      fromMaybe (defaults & server & port) . (readMaybe =<<) <$>
      lookupEnv "PORT"
    getDatabaseUrl =
      fromMaybe (defaults & database & url & unpack) <$> lookupEnv "DATABASE_URL"
