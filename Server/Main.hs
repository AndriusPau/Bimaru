{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.Aeson as AE
import Data.ByteString.Char8 as BS (unpack)
import Data.Text as T (unpack, intercalate)
import Data.Yaml as YAML (encode)
import Data.ByteString.Lazy as BSL (toStrict, fromStrict)
import Network.Socket (SockAddr (..))
import System.IO (getLine)
import Control.Concurrent (forkFinally, killThread, ThreadId)
import Control.Concurrent.Async (async, cancel, Async)
import Control.Monad (when)
import qualified Data.Map as Map

type Token = String
type Gamestate = String

games :: Map.Map Token Gamestate
games = Map.empty

app :: Application
app request respond = do
  let token = getToken request
  case requestMethod request of
    "POST" -> do
      let responseBody = YAML.encode (object ["message" .= ("Hello, World! " ++ BS.unpack (requestMethod request) ++ token :: String)])
      respond $ responseLBS status200 [(hContentType, "application/x-yaml")] (BSL.fromStrict responseBody)
    _ -> respond $ responseLBS status405 [(hContentType, "text/plain")] "Method Not Allowed"

main :: IO ()
main = do
  putStrLn "Listening on port 8080..."
  serverAsync <- async (run 8080 app)
  putStrLn "Enter a command ('shutdown' to stop the server):"
  loop serverAsync

loop :: Async () -> IO ()
loop serverAsync = do
  input <- getLine
  case input of
    "shutdown" -> cancel serverAsync
    _ -> do
      putStrLn "Invalid command"
      loop serverAsync




getToken :: Request -> Token
getToken request = T.unpack (T.intercalate "<-->" (pathInfo request))
  
