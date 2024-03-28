-- | Reimplementation of a ReaderT ExceptT monad stack to make request handling super simple

module Types.Handler where

import Types.HTTPResponse as Resp
import Types.HTTPRequest as Resq

import Data.ByteString.Char8 (ByteString)

import Control.Applicative
import Control.Monad
import Data.List (find)

newtype Handler a = Handler { runHandler :: (HTTPRequest -> IO (Either HTTPResponse a)) }

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

instance Alternative Handler where
  empty = throw err500
  h <|> h' = Handler (\req -> do
                         r <- runHandler h req
                         r' <- runHandler h' req
                         return . either (const r') pure $ r)


ask :: Handler HTTPRequest
ask = Handler (return . return)

asks :: (HTTPRequest -> a) -> Handler a
asks f = f <$> ask

route :: [ByteString] -> Handler ()
route ps = asks ((== ps) . urlPath . path) >>= guard

assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc key = fmap snd . find ((== key) . fst)

param :: ByteString -> Handler ByteString
param p = do
  ps <- asks (assoc p . params . path)
  maybe (throw err400) return $ ps


header :: ByteString -> Handler ByteString
header h = do
  hs <- asks (assoc h . Resq.headers)
  maybe (throw err400) return $ hs
