# request

![](https://miro.medium.com/max/1200/1*5KglaZoNp4fNpNHUao5u5w.jpeg)

HTTP client for haskell, inpired by [requests](https://requests.readthedocs.io/) and [http-dispatch](https://github.com/owainlewis/http-dispatch).

## Installation

This pacakge is published on [hackage](http://hackage.haskell.org/package/request) with the same name `request`, you can install it with cabal or stack or nix as any other hackage packages.

## Usage

You can try this in haskell REPL once you have `request` installed:

```haskell
import Network.HTTP.Request

resp <- get "https://api.leancloud.cn/1.1/date"
print $ responseStatus resp
```

## Record Dot Syntax Support

This library supports modern Haskell record dot syntax. To use it, enable these language extensions:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
```

### Creating Records with Dot Syntax

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Network.HTTP.Request

-- Create request using record dot syntax
let req = Request { method = GET, url = "https://api.leancloud.cn/1.1/date", headers = [], body = Nothing }
response <- send req

-- Access response fields with dot syntax
print response.status        -- 200
print response.headers       -- [("content-type","application/json")]
print response.body          -- "{\"__type\":\"Date\",\"iso\":\"2021-09-01T00:00:00.000Z\"}"
```

## Core API

Request's API has three core concepts: `Request` record type, `Response` record type, `send` function.

### Request

`Request` is all about the information you will send to the target URL.

```haskell
data Request a = Request
  { method  :: Method
  , url     :: String
  , headers :: Headers
  , body    :: Maybe a
  } deriving (Show)
```

### send

Once you have constructed your own `Request` record, you can call the `send` function to send it to the server. The `send` function's type is:

```haskell
send :: (IsString a) => Request a -> IO Response
```

### Response

`Response` is what you got from the server URL.

```haskell
data Response = Response
  { status  :: Int
  , headers :: Headers
  , body    :: Data.ByteString.ByteString
  } deriving (Show)
```

### Backward Compatibility

For users who prefer not to use the language extensions, you can still:

- Create requests using positional arguments: `Request GET "url" [] Nothing`
- Use prefixed accessor functions: `requestStatus response`, `requestHeaders response`, etc.

### Example

```haskell
:set -XOverloadedStrings

import Network.HTTP.Request

-- Construct a Request record.
let req = Request GET "https://api.leancloud.cn/1.1/date" [] Nothing
-- Send it.
res <- send req
-- Access the fields on Response.
print $ status res

-- Or with backward compatibility:
print $ responseStatus res
```

## Shortcuts

As you expected, there are some shortcuts for the most used scenarios.

```haskell
get :: String -> IO Response
get url =
  send $ Request { method = GET, url = url, headers = [], body = Nothing }

delete :: String -> IO Response
delete url =
  send $ Request { method = DELETE, url = url, headers = [], body = Nothing }

post :: (String, Maybe Data.ByteString.ByteString) -> IO Response
post (url, body) =
  send $ Request { method = POST, url = url, headers = [], body = body }

put :: (String, Maybe Data.ByteString.ByteString) -> IO Response
put (url, body) =
  send $ Request { method = PUT, url = url, headers = [], body = body }
```

These shortcuts' definitions are simple and direct. You are encouraged to add your own if the built-in does not match your use cases, like add custom headers in every request.

## API Documents

See the hackage page: http://hackage.haskell.org/package/request/docs/Network-HTTP-Request.html

## About the Project

Request is &copy; 2020-2021 by [aisk](https://github.com/aisk).

### License

Request is distributed by a [BSD license](https://github.com/aisk/request/tree/master/LICENSE).
