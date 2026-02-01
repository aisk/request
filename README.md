# request

![](https://miro.medium.com/max/1200/1*5KglaZoNp4fNpNHUao5u5w.jpeg)

HTTP client for haskell, inpired by [requests](https://requests.readthedocs.io/) and [http-dispatch](https://github.com/owainlewis/http-dispatch).

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/aisk/request)

## Installation

This pacakge is published on [hackage](http://hackage.haskell.org/package/request) with the same name `request`, you can install it with cabal or stack or nix as any other hackage packages.

## Usage

This library supports modern Haskell record dot syntax. First, enable these language extensions:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
```

Then you can use the library like this:

```haskell
import Network.HTTP.Request
import qualified Data.ByteString as BS

-- Using shortcuts
resp <- get "https://api.leancloud.cn/1.1/date"
print resp.status        -- 200

-- Or construct a Request manually
let req = Request { method = GET, url = "https://api.leancloud.cn/1.1/date", headers = [], body = Nothing }

-- Response with ByteString body
responseBS <- send req :: IO (Response BS.ByteString)
print responseBS.status        -- 200
print responseBS.body          -- ByteString response

-- Response with String body
responseStr <- send req :: IO (Response String)
print responseStr.body         -- String response
```

## Core API

Request's API has three core concepts: `Request` record type, `Response` record type, `send` function.

### Request

`Request` is all about the information you will send to the target URL.

```haskell
data Request = Request
  { method  :: Method
  , url     :: String
  , headers :: Headers
  , body    :: Maybe BS.ByteString
  } deriving (Show)
```

### send

Once you have constructed your own `Request` record, you can call the `send` function to send it to the server. The `send` function's type is:

```haskell
send :: (IsString a) => Request -> IO (Response a)
```

### Response

`Response` is what you got from the server URL.

```haskell
data Response a = Response
  { status  :: Int
  , headers :: Headers
  , body    :: a
  } deriving (Show)
```

The response body type `a` can be any type that implements the `IsString` constraint, allowing flexible handling of response data.

### Without Language Extensions

If you prefer not to use the language extensions, you can still use the library with the traditional syntax:

- Create requests using positional arguments: `Request GET "url" [] Nothing`
- Use prefixed accessor functions: `responseStatus response`, `responseHeaders response`, etc.

```haskell

import Network.HTTP.Request

-- Construct a Request using positional arguments
let req = Request GET "https://api.leancloud.cn/1.1/date" [] Nothing
-- Send it
res <- send req
-- Access the fields using prefixed accessor functions
print $ responseStatus res
```

## Shortcuts

As you expected, there are some shortcuts for the most used scenarios.

```haskell
get :: (IsString a) => String -> IO (Response a)
get url =
  send $ Request GET url [] Nothing

delete :: (IsString a) => String -> IO (Response a)
delete url =
  send $ Request DELETE url [] Nothing

post :: (IsString a) => String -> Maybe BS.ByteString -> IO (Response a)
post url body =
  send $ Request POST url [] body

put :: (IsString a) => String -> Maybe BS.ByteString -> IO (Response a)
put url body =
  send $ Request PUT url [] body

patch :: (IsString a) => String -> Maybe BS.ByteString -> IO (Response a)
patch url body =
  send $ Request PATCH url [] body
```

These shortcuts' definitions are simple and direct. You are encouraged to add your own if the built-in does not match your use cases, like add custom headers in every request.

## API Documents

See the hackage page: http://hackage.haskell.org/package/request/docs/Network-HTTP-Request.html

## About the Project

Request is &copy; 2020-2026 by [AN Long](https://github.com/aisk).

### License

Request is distributed by a [BSD license](https://github.com/aisk/request/tree/master/LICENSE).
