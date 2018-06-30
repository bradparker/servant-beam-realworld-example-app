module RealWorld.Conduit.Options
  ( Options(..)
  , getOptions
  ) where

import Control.Applicative ((<*>))
import Control.Monad ((=<<))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe, maybe)
import Data.Monoid ((<>), mempty)
import Data.String (String)
import qualified Data.Text as Text
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
import Web.JWT (Secret, secret)

data Options = Options
  { port :: Int
  , databaseUrl :: ByteString
  , authSecret :: Secret
  } deriving (Show)

optionsParser :: Maybe Int -> Maybe String -> Maybe String -> Parser Options
optionsParser defaultPort defaultDatabaseUrl defaultAuthSecret =
  Options
    <$> portParser
    <*> databaseUrlParser
    <*> authSecretParser
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
    authSecretParser =
      secret . Text.pack <$> option str
        ( maybe mempty value defaultAuthSecret
        <> short 's'
        <> long "auth-secret"
        <> help "A very big, very secret, very random string"
        <> metavar "AUTH_SECRET"
        )

getOptions :: IO Options
getOptions = do
  port <- getPort
  databaseUrl <- getDatabaseUrl
  authSecret <- getAuthSecret
  customExecParser
    (prefs showHelpOnError)
    (info
       (helper <*> optionsParser port databaseUrl authSecret)
       (header "Realworld Conduit" <> fullDesc))
  where
    getPort = (readMaybe =<<) <$> lookupEnv "PORT"
    getDatabaseUrl = lookupEnv "DATABASE_URL"
    getAuthSecret = lookupEnv "AUTH_SECRET"
