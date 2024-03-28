{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Types.HTTPRequest
import Types.HTTPResponse
import ReadP.ByteString
import Types.Handler
import Control.Applicative

readHTTPRequest :: Socket -> IO (Maybe HTTPRequest)
readHTTPRequest sock = do
    d <- readAll sock
    return $ parseRequest d
  where
    readAll s = do
        chunk <- recv s 1024
        if S.length chunk < 1024 then
            return chunk
        else (chunk <>) <$> readAll s

serverHandler :: Handler HTTPResponse
serverHandler =
  (prefix ["echo"] >> fmap (text . S.intercalate "/" . tail) reqPath)
  <|> (route ["user-agent"] >> fmap text (header "User-Agent"))
  <|> (route [] >> return ok200)
  <|> throw err404

main :: IO ()
main = runTCPServer Nothing "4221" talk
  where
    talk s = do
        readR <- readHTTPRequest s
        case readR of
            Just req -> do
              resp <- runHandler serverHandler req
              sendAll s (render . either id id $ resp)
            Nothing -> sendAll s (render err400)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)
