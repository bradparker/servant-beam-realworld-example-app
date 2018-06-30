module RealWorld.Conduit.Web.Namespace
  ( Namespace(..)
  , unNamespace
  ) where

import Control.Applicative (pure)
import Control.Lens ((.~))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Function (($), (&), (.))
import Data.Functor ((<$>))
import qualified Data.HashMap.Strict.InsOrd as HashMap
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger
  ( NamedSchema(NamedSchema)
  , SwaggerType(SwaggerObject)
  , ToSchema(..)
  , declareSchemaRef
  , properties
  , required
  , type_
  )
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

newtype Namespace (ns :: Symbol) a =
  Namespace a

unNamespace :: Namespace ns a -> a
unNamespace (Namespace a) = a

symbolToText :: KnownSymbol a => Proxy a -> Text
symbolToText = Text.pack . symbolVal

instance (KnownSymbol ns, ToJSON a) => ToJSON (Namespace ns a) where
  toJSON (Namespace a) = object [symbolToText (Proxy :: Proxy ns) .= a]

instance (KnownSymbol ns, FromJSON a) => FromJSON (Namespace ns a) where
  parseJSON =
    withObject "Namespace" $ \v ->
      Namespace <$> v .: symbolToText (Proxy :: Proxy ns)

instance (KnownSymbol ns, ToSchema a) => ToSchema (Namespace ns a) where
  declareNamedSchema _ = do
    contentSchema <- declareSchemaRef (Proxy :: Proxy a)
    pure $
      NamedSchema Nothing $
      mempty
        & type_ .~ SwaggerObject
        & properties .~ HashMap.fromList [(key, contentSchema)]
        & required .~ [key]
    where
      key = symbolToText (Proxy :: Proxy ns)
