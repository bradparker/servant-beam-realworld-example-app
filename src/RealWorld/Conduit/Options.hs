module RealWorld.Conduit.Options
  ( Options(..)
  , getOptions
  ) where

import Control.Applicative ((<*>))
import Control.Monad ((=<<))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>), mempty)
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

data Options = Options
  { port :: Int
  , databaseUrl :: ByteString
  } deriving (Show)

optionsParser :: Maybe Int -> Maybe String -> Parser Options
optionsParser defaultPort defaultDatabaseUrl =
  Options <$> portParser <*> databaseUrlParser
  where
    portParser =
      option auto
        ( maybe (value 8080) value defaultPort
        <> short 'p'
        <> long "port"
        <> metavar "PORT"
        )
    databaseUrlParser =
      pack <$> option str
        ( maybe mempty value defaultDatabaseUrl
        <> short 'd'
        <> long "database-url"
        <> help "Of the form postgres://user:password@hostname:port/name"
        <> metavar "DATABASE_URL"
        )

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
    getPort = (readMaybe =<<) <$> lookupEnv "PORT"
    getDatabaseUrl = lookupEnv "DATABASE_URL"
