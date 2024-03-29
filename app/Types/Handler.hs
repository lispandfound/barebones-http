{-# LANGUAGE OverloadedStrings #-}

-- | Reimplementation of a ReaderT ExceptT monad stack to make request handling super simple
module Types.Handler (RequestParser, Handler, route, param, prefix, reqPath, suff, header, method, contentType, runParser, (<||>)) where

import Control.Monad.Except
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.List (find, isPrefixOf)
import Types.HTTPRequest qualified as Req
import Types.HTTPResponse as Resp

type RequestHandler m a = ReaderT Req.HTTPRequest (ExceptT HTTPResponse m) a

type Handler a = RequestHandler IO a

type RequestParser a = RequestHandler Identity a

runHandler :: Handler HTTPResponse -> Req.HTTPRequest -> IO HTTPResponse
runHandler handler = either return return <=< (runExceptT . runReaderT handler)

runParser :: RequestParser (Handler HTTPResponse) -> Req.HTTPRequest -> IO HTTPResponse
runParser parser req = either return (`runHandler` req) . runIdentity . runExceptT . runReaderT parser $ req

-- This function specifies the order in which we should report errors among a
-- sequence of errors. Given a number of RequestParsers we can get a range of
-- different errors. We want to pick the error that most closely matches the
-- user's expectation.  For example: We may a sequence of parsers h1, h2, and
-- h3. h1 returns err404 because the route doesn't match, h2 returns a 405
-- because the method doesn't match and h3 returns 415 because the content type
-- doesn't match. In this situation the user was likely trying to activate the
-- h3 endpoint but failed because of the content type. It's not a complete
-- solution, but it's good enough.
higherPriority :: StatusCode -> StatusCode -> Bool
-- 404 is the lowest priority error
(StatusCode 404 _) `higherPriority` _ = False
-- 405 is lower than all error codes besides 404
(StatusCode 405 _) `higherPriority` (StatusCode 404 _) = True
(StatusCode 405 _) `higherPriority` _ = False
-- 415 is lower than all priorities besides 404 and 405
(StatusCode 415 _) `higherPriority` (StatusCode 404 _) = True
(StatusCode 415 _) `higherPriority` (StatusCode 405 _) = True
(StatusCode 415 _) `higherPriority` _ = False
-- Otherwise, we assume that the first error
_ `higherPriority` _ = True

maxError :: HTTPResponse -> HTTPResponse -> HTTPResponse
maxError r r' = if status r `higherPriority` status r' then r else r'

infixl 3 <||>

-- This doesn't meet the strict definition of an alternative so we don't write it an alternative instance
(<||>) :: RequestParser a -> RequestParser a -> RequestParser a
h <||> h' = h `catchError` (\r -> h' `catchError` (throwError . maxError r))

guardError :: (MonadError e m) => e -> Bool -> m ()
guardError e False = throwError e
guardError _ _ = return ()

route :: (Monad m) => [ByteString] -> RequestHandler m ()
route ps = reqPath >>= guardError err404 . (ps ==)

assoc :: (Eq a) => a -> [(a, b)] -> Maybe b
assoc key = fmap snd . find ((== key) . fst)

param :: (Monad m) => ByteString -> RequestHandler m ByteString
param p = do
  ps <- asks (assoc p . Req.headers)
  maybe (throwError err400) return ps

prefix :: (Monad m) => [ByteString] -> RequestHandler m ()
prefix ps = reqPath >>= guardError err404 . (ps `isPrefixOf`)

reqPath :: (Monad m) => RequestHandler m [ByteString]
reqPath = asks (Req.urlPath . Req.path)

suff :: (Monad m) => RequestHandler m ByteString
suff = last <$> reqPath

header :: (Monad m) => ByteString -> RequestHandler m ByteString
header h = do
  hs <- asks (assoc h . Req.headers)
  maybe (throwError err400) return $ hs

method :: (Monad m) => Req.HTTPMethod -> RequestHandler m ()
method m = do
  matchesMethod <- asks ((== m) . Req.method)
  if matchesMethod then return () else throwError err405

contentType :: (Monad m) => ByteString -> RequestHandler m ()
contentType mime = asks ((== Just mime) . assoc "Content-Type" . Req.headers) >>= guardError err415
