-- | Reimplementation of a ReaderT ExceptT monad stack to make request handling super simple
{-# LANGUAGE OverloadedStrings #-}
module Types.Handler (Handler (..), throw, liftIO, ask, asks, route, param, prefix, reqPath, suff, header, method, contentType) where

import Types.HTTPResponse as Resp
import qualified Types.HTTPRequest as Resq

import Data.ByteString.Char8 (ByteString)

import Control.Applicative
import Control.Monad
import Data.List (find, isPrefixOf)

newtype Handler a = Handler { runHandler :: (Resq.HTTPRequest -> IO (Either HTTPResponse a)) }

instance Functor Handler where
  fmap f h = Handler $ fmap (fmap f) . (runHandler h)

instance Applicative Handler where
  pure = Handler . const . return . return
  fh <*> h = Handler $ (\r -> do
                          f <- runHandler fh r
                          h' <- runHandler h r
                          return $ f <*> h')

instance Monad Handler where
  return = pure
  h >>= f = Handler $ (\r -> do
                          h' <- runHandler h r
                          case h' of
                            Left e -> return (Left e)
                            Right v -> runHandler (f v) r)


throw :: HTTPResponse -> Handler a
throw = Handler . const . return . Left

is404 :: Either HTTPResponse a -> Bool
is404 (Right _) = False
is404 (Left r) = status r == status404

instance Alternative Handler where
  empty = throw err404
  h <|> h' = Handler (\req -> do
                         r <- runHandler h req
                         r' <- runHandler h' req
                         case r of
                           Left e | status e `notElem` [status404, status405] -> return (Left e)
                           Left e | status e == status405 && is404 r' -> return (Left e)
                           Left e | status e `elem` [status404, status405] -> return r'
                           Left e -> return (Left e)
                           Right v -> return (Right v))


liftIO :: IO a -> Handler a
liftIO act = Handler (\_ -> act >>= return . Right)

ask :: Handler Resq.HTTPRequest
ask = Handler (return . return)

asks :: (Resq.HTTPRequest -> a) -> Handler a
asks f = f <$> ask

route :: [ByteString] -> Handler ()
route ps = (== ps) <$> reqPath >>= guard

assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc key = fmap snd . find ((== key) . fst)

param :: ByteString -> Handler ByteString
param p = do
  ps <- assoc p <$> asks Resq.headers
  maybe (throw err400) return $ ps

prefix :: [ByteString] -> Handler ()
prefix ps = (ps `isPrefixOf`) <$> reqPath >>= guard

reqPath :: Handler [ByteString]
reqPath = asks (Resq.urlPath . Resq.path)

suff :: Handler ByteString
suff = last <$> reqPath

header :: ByteString -> Handler ByteString
header h = do
  hs <- asks (assoc h . Resq.headers)
  maybe (throw err400) return $ hs

method :: Resq.HTTPMethod -> Handler ()
method m = do
  matchesMethod <- asks ((== m) . Resq.method)
  if matchesMethod then return () else throw err405


contentType :: ByteString -> Handler ()
contentType mime = asks (maybe False (== mime) . assoc "Content-Type" . Resq.headers) >>= guard
