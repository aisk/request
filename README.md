# request

![](https://miro.medium.com/max/1200/1*5KglaZoNp4fNpNHUao5u5w.jpeg)

HTTP client for haskell, inpired by [requests](https://requests.readthedocs.io/) and [http-dispatch](https://github.com/owainlewis/http-dispatch).

## Usage

You can try this in haskell REPL once you have `request` installed:

```haskell
import Network.HTTP.Request

resp <- get "https://api.leancloud.cn/1.1/date"
print $ requestStatus resp
```

## Core API

Request's API has three core concepts: `Request` record type, `Response` record type, `send` function.

### Request

`Request` is all about the information you will send to the target URL.

```haskell
data Request = Request
  { requestMethod  :: Method
  , requestUrl     :: String
  , requestHeaders :: Headers
  , requestBody    :: Maybe Data.ByteString.ByteString
  } deriving (Show)
```

### send

Once you have constructed your own `Request` record, you can call the `send` function to send it to the server. The `send` function's type is:

```haskell
send :: Request -> IO Response
```

### Response

`Response` is what you got from the server URL.

```haskell
data Response = Response
  { responseStatus  :: Int
  , responseHeaders :: Headers
  , responseBody    :: Data.ByteString.ByteString
  } deriving (Show)
```

### Example

```haskell
:set -XOverloadedStrings

import Network.HTTP.Request

-- Construct a Request record.
let req = Request GET "https://api.leancloud.cn/1.1/date" [] Nothing
-- Send it.
res <- send req
-- access the fields on Response.
print $ requestStatus resp
```

## Shortcuts

As you expected, there are some shortcuts for the most used scenarios.

```haskell
get :: String -> IO Response
get url =
  send $ Request GET url [] Nothing

delete :: String -> IO Response
delete url =
  send $ Request DELETE url [] Nothing

post :: (String, Maybe Data.ByteString.ByteString) -> IO Response
post (url, body) =
  send $ Request POST url [] body

put :: (String, Maybe Data.ByteString.ByteString) -> IO Response
put (url, body) =
  send $ Request PUT url [] body
```

These shortcuts' definitions are simple and direct. You are encouraged to add your own if the built-in does not match your use cases, like add custom headers in every request.

## About the Project

Request is &copy; 2020-2021 by [aisk](https://github.com/aisk).

### License

Request is distributed by a [BSD license](https://github.com/aisk/request/tree/master/LICENSE).
