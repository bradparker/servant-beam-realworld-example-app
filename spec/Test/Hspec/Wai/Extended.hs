module Test.Hspec.Wai.Extended
  ( module Test.Hspec.Wai
  , post'
  , put'
  , get'
  , delete'
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Network.HTTP.Types
  ( Header
  , hContentType
  , methodDelete
  , methodGet
  , methodPost
  , methodPut
  )
import Network.Wai.Test (SResponse)
import Test.Hspec.Wai

post' :: ByteString -> [Header] -> Lazy.ByteString -> WaiSession SResponse
post' path headers =
  request methodPost path ((hContentType, "application/json") : headers)

put' :: ByteString -> [Header] -> Lazy.ByteString -> WaiSession SResponse
put' path headers =
  request methodPut path ((hContentType, "application/json") : headers)

get' :: ByteString -> [Header] -> WaiSession SResponse
get' path headers =
  request methodGet path ((hContentType, "application/json") : headers) ""

delete' :: ByteString -> [Header] -> WaiSession SResponse
delete' path headers =
  request methodDelete path ((hContentType, "application/json") : headers) ""
