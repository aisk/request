{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request
  ( Header,
    Headers,
    Method (..),
    Request (..),
    Response (..),
    get,
    patch,
    post,
    put,
    send,
    requestMethod,
    requestUrl,
    requestHeaders,
    requestBody,
    responseStatus,
    responseHeaders,
    responseBody,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.String as S
import qualified Network.HTTP.Client as LowLevelClient
import qualified Network.HTTP.Client.TLS as LowLevelTLSClient
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
  show DELETE = "DELETE"
  show GET = "GET"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show PATCH = "PATCH"
  show POST = "POST"
  show PUT = "PUT"
  show TRACE = "TRACE"
  show (Method method) = method

data Request = Request
  { method :: Method,
    url :: String,
    headers :: Headers,
    body :: Maybe BS.ByteString
  }
  deriving (Show)

-- Compatibility accessor functions
requestMethod :: Request -> Method
requestMethod req = req.method

requestUrl :: Request -> String
requestUrl req = req.url

requestHeaders :: Request -> Headers
requestHeaders req = req.headers

requestBody :: Request -> Maybe BS.ByteString
requestBody req = req.body

toLowlevelRequest :: Request -> IO LowLevelClient.Request
toLowlevelRequest req = do
  initReq <- LowLevelClient.parseRequest req.url
  return $
    initReq
      { LowLevelClient.method = C.pack . show $ req.method,
        LowLevelClient.requestHeaders = map (\(k, v) -> (CI.mk k, v)) req.headers,
        LowLevelClient.requestBody = maybe mempty LowLevelClient.RequestBodyBS req.body
      }

data Response a = Response
  { status :: Int,
    headers :: Headers,
    body :: a
  }
  deriving (Show)

-- Compatibility accessor functions for Response
responseStatus :: Response a -> Int
responseStatus res = res.status

responseHeaders :: Response a -> Headers
responseHeaders res = res.headers

responseBody :: Response a -> a
responseBody res = res.body

fromLowLevelResponse :: (S.IsString a) => LowLevelClient.Response LBS.ByteString -> Response a
fromLowLevelResponse res =
  let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ res
      body = S.fromString . C.unpack . LBS.toStrict $ LowLevelClient.responseBody res
      headers = LowLevelClient.responseHeaders res
   in Response
        status
        ( map
            ( \(k, v) ->
                let hk = CI.original k
                 in (hk, v)
            )
            headers
        )
        body

send :: (S.IsString a) => Request -> IO (Response a)
send req = do
  manager <- LowLevelTLSClient.getGlobalManager
  llreq <- toLowlevelRequest req
  llres <- LowLevelClient.httpLbs llreq manager
  return $ fromLowLevelResponse llres

get :: (S.IsString a) => String -> IO (Response a)
get url =
  send $ Request GET url [] Nothing

delete :: (S.IsString a) => String -> IO (Response a)
delete url =
  send $ Request DELETE url [] Nothing

post :: (S.IsString a) => String -> Maybe BS.ByteString -> IO (Response a)
post url body =
  send $ Request POST url [] body

put :: (S.IsString a) => String -> Maybe BS.ByteString -> IO (Response a)
put url body =
  send $ Request PUT url [] body

patch :: (S.IsString a) => String -> Maybe BS.ByteString -> IO (Response a)
patch url body =
  send $ Request PATCH url [] body
