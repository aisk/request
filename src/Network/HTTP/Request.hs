{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request
  ( Header
  , Headers
  , Method (..)
  , Request (..)
  , Response (..)
  , get
  , post
  , put
  , send
  ) where

import qualified Data.String               as S
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.CaseInsensitive      as CI
import qualified Data.List                 as List
import qualified Network.HTTP.Client       as LowLevelClient
import qualified Network.HTTP.Client.TLS   as LowLevelTLSClient
import qualified Network.HTTP.Types.Status as LowLevelStatus

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
  | Method String

instance Show Method where
  show GET = "GET"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show PATCH = "PATCH"
  show POST = "POST"
  show PUT = "PUT"
  show TRACE = "TRACE"
  show (Method method) = method

data (S.IsString a) => Request a = Request
  { requestMethod  :: Method
  , requestUrl     :: String
  , requestHeaders :: Headers
  , requestBody    :: Maybe a
  } deriving (Show)

toLowlevelRequest :: (S.IsString a) => Request a -> IO LowLevelClient.Request
toLowlevelRequest req = do
  initReq <- LowLevelClient.parseRequest $ requestUrl req
  return $ initReq { LowLevelClient.method = C.pack . show $ requestMethod req
                   , LowLevelClient.requestHeaders = map (\(k, v) -> (CI.mk k, v)) $ requestHeaders req
                   }

data Response = Response
  { responseStatus  :: Int
  , responseHeaders :: Headers
  , responseBody    :: BS.ByteString
  } deriving (Show)

fromLowLevelRequest :: LowLevelClient.Response LBS.ByteString -> Response
fromLowLevelRequest res =
  let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ res
      body = LBS.toStrict $ LowLevelClient.responseBody res
      headers = LowLevelClient.responseHeaders res
  in
  Response status (map (\(k,v) ->
                         let hk = CI.original k
                         in
                         (hk, v)) headers) body

send :: (S.IsString a) => Request a -> IO Response
send req = do
  manager <- LowLevelTLSClient.getGlobalManager
  llreq <- toLowlevelRequest req
  llres <- LowLevelClient.httpLbs llreq manager
  return $ fromLowLevelRequest llres

get :: String -> IO Response
get url =
  send $ Request GET url [] Nothing

delete :: String -> IO Response
delete url =
  send $ Request DELETE url [] Nothing

post :: (String, Maybe BS.ByteString) -> IO Response
post (url, body) =
  send $ Request POST url [] body

put :: (String, Maybe BS.ByteString) -> IO Response
put (url, body) =
  send $ Request PUT url [] body
