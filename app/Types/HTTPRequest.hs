{-# LANGUAGE OverloadedStrings #-}

-- | Types for an HTTP Request
module Types.HTTPRequest where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char (isAlphaNum)
import Data.Functor (($>))

data HTTPMethod = GET | POST | PUT | DELETE deriving (Show, Eq)

data HTTPVersion = HTTP11 deriving (Show, Eq)

data HTTPRequest = HTTPRequest
  { method :: HTTPMethod,
    path :: URL,
    version :: HTTPVersion,
    headers :: [(ByteString, ByteString)],
    body :: ByteString
  }
  deriving (Show)

data URL = URL
  { urlPath :: [ByteString],
    params :: [(ByteString, ByteString)]
  }
  deriving (Show)

isSafe :: Char -> Bool
isSafe = (`elem` ['$', '-', '_', '@', '.', '&', '+', '-'])

url :: Parser URL
url = URL <$> (char '/' >> (component `sepBy` char '/')) <*> option [] pParam
  where
    component = takeWhile1 (\c -> isAlphaNum c || isSafe c)
    pParam = char '?' >> (keyvalue `sepBy` "&")
    keyvalue = (,) <$> key <*> ("=" >> value)
    key = takeWhile1 isAlphaNum
    value = key

parseRequest :: ByteString -> Maybe HTTPRequest
parseRequest = either (const Nothing) Just . parseOnly request
  where
    crlf = void $ string "\r\n"
    request = do
      m <- pMethod
      void space
      p <- url
      void space
      v <- pVersion
      crlf
      hs <- pHeader
      crlf
      crlf
      b <- takeByteString
      return $
        HTTPRequest
          { method = m,
            path = p,
            version = v,
            headers = hs,
            body = b
          }
    pMethod = choice [string "GET" $> GET, string "POST" $> POST, string "PUT" $> PUT, string "DELETE" $> DELETE]
    pVersion = string "HTTP/1.1" $> HTTP11
    pHeader = keyvalue `sepBy1` crlf
    isExtra = (`elem` ("!#$&'()*+,/:;=?@[]%-_.~" :: String))
    keyvalue = (,) <$> key <*> (string ": " >> value)
    value = takeWhile1 (\c -> isAlphaNum c || isExtra c)
    key = takeWhile1 (\c -> isAlphaNum c || isSafe c)
