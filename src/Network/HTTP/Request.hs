module Network.HTTP.Request
  ( Request
  , Response
  , Header
  , Headers
  ) where

import qualified Data.ByteString as BS

type Header = (BS.ByteString, BS.ByteString)

type Headers = [Header]

data Method
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT
  | TRACE
  deriving (Eq, Show)

data Request = Request
  { requestMethod  :: Method
  , requestUrl     :: String
  , requestHeaders :: Headers
  , requestBody    :: Maybe BS.ByteString
  } deriving (Show)

data Response = Response
  { responseStatus  :: Int
  , responseHeaders :: Headers
  , responseBody    :: BS.ByteString
  } deriving (Show)
