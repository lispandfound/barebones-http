{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Types for an HTTP Response

module Types.HTTPResponse (HTTPResponse (..), ok200, render)  where

import Data.ByteString.Char8 as B

data StatusCode = StatusCode Int ByteString

status200 :: StatusCode
status200 = StatusCode 200 "OK"

data HTTPResponse = HTTPResponse {
  status :: StatusCode,
  headers :: [(ByteString, ByteString)],
  body :: ByteString
  }


statusLine :: StatusCode -> ByteString
statusLine (StatusCode code status) = "HTTP/1.1 " <> (B.pack . show) code <> " " <> status

render :: HTTPResponse -> ByteString
render (HTTPResponse {..}) = B.unlines [statusLine status,
                                        joinHeaders headers,
                                        "",
                                        body]
  where
    joinHeaders = foldMap (\(k, v) -> k <> ": " <> v <> "\n")

ok200 :: HTTPResponse
ok200 = HTTPResponse {
  status = status200,
  headers = [],
  body = mempty
                     }
