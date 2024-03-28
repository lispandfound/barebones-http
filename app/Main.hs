{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Char8 as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Types.HTTPRequest (HTTPRequest, parseRequest, HTTPMethod(..))
import Types.HTTPResponse
import ReadP.ByteString
import Types.Handler
import Control.Applicative
import System.Console.GetOpt
import System.Exit (exitFailure)
import System.Environment (getArgs)

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

serverHandler :: FilePath -> Handler HTTPResponse
serverHandler dir =
  (prefix ["echo"] >> method GET >>  fmap (text . S.intercalate "/" . tail) reqPath)
  <|> ( route ["user-agent"] >> method GET >> fmap text (header "User-Agent"))
  <|> ( prefix ["files"] >> method POST >> contentType "application/octet-stream" >> postFile dir)
  <|> ( prefix ["files"] >> method GET >> serveFile dir)
  <|> ( route [] >> method GET >> return ok200)
  <|> empty

readFileResp :: FilePath -> IO HTTPResponse
readFileResp path = (octet <$> S.readFile path) `E.catch` err
  where
    err :: E.SomeException -> IO HTTPResponse
    err = const $ return err404

serveFile :: FilePath -> Handler HTTPResponse
serveFile dir = do
  suffixPath <- fmap (S.unpack . S.intercalate "/" . tail) reqPath
  let fullPath = dir <> "/" <> suffixPath
  resp <- liftIO $ readFileResp fullPath
  return resp

data Flag = Directory FilePath deriving (Show)

options :: [ OptDescr Flag ]
options = [ Option [] ["directory"] (ReqArg Directory "DIR") "Output directory to read files from"]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))
    where usage = "Usage: http-server --directory DIR"


main :: IO ()
main = do
  argv <- getArgs
  opts <- compilerOpts argv
  case opts of
    ([Directory dir], _) -> runServer dir
    _ -> exitFailure


runServer :: FilePath -> IO ()
runServer dir = runTCPServer Nothing "4221" talk
  where
    talk s = do
        readR <- readHTTPRequest s
        case readR of
            Just req -> do
              resp <- runHandler (serverHandler dir) req
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
