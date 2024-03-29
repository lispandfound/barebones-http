{-# LANGUAGE OverloadedStrings #-}

-- | Types for an HTTP Request
module Types.HTTPRequest where

import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.List (find)
import ReadP.ByteString

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

url :: ReadP URL
url = URL <$> (char '/' >> (component `sepBy` char '/')) <*> (option [] pParam)
  where
    component = munch1 (\c -> isAlphaNum c || isSafe c)
    pParam = char '?' >> (keyvalue `sepBy` char '&')
    keyvalue = (,) <$> key <*> (char '=' >> value)
    key = munch isAlphaNum
    value = key

parseRequest :: ByteString -> Maybe HTTPRequest
parseRequest = fmap fst . find (B.null . snd) . parseRequestS

parseRequestS :: ByteString -> [(HTTPRequest, ByteString)]
parseRequestS = readP_to_S request
  where
    space = void $ char ' '
    crlf = void $ string "\r\n"
    request = do
      m <- pMethod
      space
      p <- url
      space
      v <- pVersion
      crlf
      hs <- pHeader
      crlf
      crlf
      b <- rest
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
    pHeader = keyvalue `sepBy` crlf
    isExtra = (`elem` ("!#$&'()*+,/:;=?@[]%-_.~" :: String))
    keyvalue = (,) <$> key <*> (string ": " >> value)
    value = munch1 (\c -> isAlphaNum c || isExtra c)
    key = munch1 (\c -> isAlphaNum c || isSafe c)
