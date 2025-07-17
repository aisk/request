{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Request
import Test.Hspec

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
