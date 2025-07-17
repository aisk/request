{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Request
import Test.Hspec
import qualified Data.ByteString as BS

main :: IO ()
main = hspec $ do
  describe "Network.HTTP.Request" $ do
    it "should fetch example.com and return 200 OK" $ do
      response <- get "http://example.com"
      responseStatus response `shouldBe` 200

    it "should send a request to example.com and return 200 OK" $ do
      response <- send (Request GET "http://example.com" [] Nothing)
      responseStatus response `shouldBe` 200

    it "should post to httpbin.org/post and return 200 OK" $ do
      response <- post ("https://postman-echo.com/post", Just "Hello!")
      responseStatus response `shouldBe` 200

    it "should put to httpbin.org/put and return 200 OK" $ do
      response <- put ("https://postman-echo.com/put", Just "Hello!")
      responseStatus response `shouldBe` 200

    it "should patch to httpbin.org/patch and return 200 OK" $ do
      response <- patch ("https://postman-echo.com/patch", Just "Hello!")
      responseStatus response `shouldBe` 200

  it "should use dot record syntax to create and access request/response" $ do
    let req = Request { method = GET, url = "http://example.com", headers = [("User-Agent", "Haskell-Request")], body = Nothing }
    response <- send req
    response.status `shouldBe` 200
    response.headers `shouldSatisfy` (not . null)

  it "should access response body with different IsString types" $ do
    -- Test with ByteString body
    let req1 = Request { method = GET, url = "http://example.com", headers = [], body = Nothing }
    response1 <- send req1 :: IO (Response BS.ByteString)
    BS.length response1.body `shouldSatisfy` (> 0)

    -- Test with String body
    let req2 = Request { method = GET, url = "http://example.com", headers = [], body = Nothing }
    response2 <- send req2 :: IO (Response String)
    not (null response2.body) `shouldBe` True
