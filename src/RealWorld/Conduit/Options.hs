module RealWorld.Conduit.Options
  ( Options(..)
  , getOptions
  , octKey
  ) where

import Control.Applicative ((<*>))
import Control.Monad ((=<<))
import Crypto.JOSE
  ( JWK
  , KeyMaterial(OctKeyMaterial)
  , OctKeyParameters(OctKeyParameters)
  , fromKeyMaterial
  )
import Crypto.JOSE.Types (Base64Octets(Base64Octets))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Function ((.))
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
  , authKey :: JWK
  } deriving (Show)

optionsParser :: Maybe Int -> Maybe String -> Maybe String -> Parser Options
optionsParser defaultPort defaultDatabaseUrl defaultAuthKey =
  Options
    <$> portParser
    <*> databaseUrlParser
    <*> authKeyParser
  where
    portParser =
      option auto
        ( maybe (value 8080) value defaultPort
        <> short 'p'
        <> long "port"
        <> metavar "PORT"
        )
    databaseUrlParser =
      ByteString.pack <$> option str
        ( maybe mempty value defaultDatabaseUrl
        <> short 'd'
        <> long "database-url"
        <> help "Of the form postgres://user:password@hostname:port/name"
        <> metavar "DATABASE_URL"
        )
    authKeyParser =
      octKey <$> option str
        ( maybe mempty value defaultAuthKey
        <> short 's'
        <> long "auth-secret"
        <> help "256 random chars."
        <> metavar "AUTH_SECRET"
        )

octKey :: String -> JWK
octKey =
  fromKeyMaterial .
  OctKeyMaterial .
  OctKeyParameters .
  Base64Octets .
  ByteString.pack

getOptions :: IO Options
getOptions = do
  port <- getPort
  databaseUrl <- getDatabaseUrl
  authKey <- getAuthKey
  customExecParser
    (prefs showHelpOnError)
    (info
       (helper <*> optionsParser port databaseUrl authKey)
       (header "Realworld Conduit" <> fullDesc))
  where
    getPort = (readMaybe =<<) <$> lookupEnv "PORT"
    getDatabaseUrl = lookupEnv "DATABASE_URL"
    getAuthKey = lookupEnv "AUTH_SECRET"
