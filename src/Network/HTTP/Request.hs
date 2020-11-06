{-# LANGUAGE DuplicateRecordFields #-}

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
  { method  :: Method
  , url     :: String
  , headers :: Headers
  , body    :: Maybe BS.ByteString
  } deriving (Show)

data Response = Response
  { status  :: Int
  , headers :: Headers
  , body    :: BS.ByteString
  } deriving (Show)
