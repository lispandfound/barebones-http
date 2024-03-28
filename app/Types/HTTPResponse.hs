{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Types for an HTTP Response

module Types.HTTPResponse (HTTPResponse (..), ok200, err400, err500, err404, err405, render, text, octet)  where

import Data.ByteString.Char8 as B

data StatusCode = StatusCode Int ByteString

status200 :: StatusCode
status200 = StatusCode 200 "OK"

status400 :: StatusCode
status400 = StatusCode 400 "Bad Request"

status404 :: StatusCode
status404 = StatusCode 404 "Not Found"

status500 :: StatusCode
status500 = StatusCode 500 "Internal Server Error"

status405 :: StatusCode
status405 = StatusCode 405 "Method Not Allowed"

data HTTPResponse = HTTPResponse {
  status :: StatusCode,
  headers :: [(ByteString, ByteString)],
  body :: ByteString
  }



statusLine :: StatusCode -> ByteString
statusLine (StatusCode code status) = "HTTP/1.1 " <> (B.pack . show) code <> " " <> status

render :: HTTPResponse -> ByteString
render (HTTPResponse {..}) = uncrlf [statusLine status,
                                        joinHeaders headers,
                                        body]
  where
    joinHeaders = foldMap (\(k, v) -> k <> ": " <> v <> "\r\n")
    uncrlf = B.intercalate "\r\n"

ok200 :: HTTPResponse
ok200 = HTTPResponse {
  status = status200,
  headers = [],
  body = mempty
                     }

err400 :: HTTPResponse
err400 = HTTPResponse {
  status = status400,
  headers = [],
  body = mempty
                       }

err404 :: HTTPResponse
err404 = HTTPResponse {
  status = status404,
  headers = [],
  body = mempty
                       }

err500 :: HTTPResponse
err500 = HTTPResponse {
  status = status500,
  headers = [],
  body = mempty
                      }


err405 :: HTTPResponse
err405 = HTTPResponse {
  status = status405,
  headers = [],
  body = mempty
                      }

text :: ByteString -> HTTPResponse
text b = ok200 {
  headers = [ ("Content-Type", "text/plain"), ("Content-Length", (B.pack . show . B.length) b) ],
  body = b }

octet :: ByteString -> HTTPResponse
octet b = ok200 {
  headers = [ ("Content-Type", "application/octet-stream"), ("Content-Length", (B.pack . show . B.length) b) ],
  body = b }
