{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.Aeson as AE
import Data.ByteString.Char8 as BS (unpack)
import Data.Text as T (pack, unpack, intercalate, Text)
import Data.Yaml as YAML (encode)
import Data.Maybe (isJust)
import Data.Functor
import Data.ByteString.Lazy as BSL (toStrict, fromStrict)
import Network.Socket (SockAddr (..))
import System.IO (getLine)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (forkFinally, killThread, ThreadId)
import Control.Concurrent.Async (async, cancel, Async)
import Control.Monad (when, liftM)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Map as Map
import GHC.IO (unsafePerformIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Lib (State (..), emptyState, toggle, render)

{-# NOINLINE games #-}

type Token = String

type Game = Map.Map Token State

games :: IORef Game
games = unsafePerformIO (newIORef Map.empty)

app :: Application
app request respond = do
  let token = getToken request
  case requestMethod request of
    "POST" -> do
      putStrLn ("\nReceived Request: \n" ++ show request ++ "\n\n")
      if not (gameExists token)
        then do
          putStrLn "New game detected!"
          addGame token
          let result = object ["game_setup_id" .= ("3a7a8f44-b224-40ff-9c5c-58a1b60eab4b" :: String),
                              "occupied_rows" .= ([1, 1, 2, 3, 1, 4, 2, 4, 2, 0] :: [Int]),
                              "occupied_cols" .= ([2, 0, 2, 2, 2, 0, 6, 0, 3, 3] :: [Int])]
          let responseBody = YAML.encode result
          respond $ responseLBS status200 [(hContentType, "application/x-yaml")] (BSL.fromStrict responseBody)
        else do
          if urlContainsShow (map T.unpack(pathInfo request)) token
          then do
            putStrLn "Not navigating blind now, are we?"
            let st = getState token
            -- print st
            let rows = getStatePart st "occupied_rows"
            -- print rows
            let cols = getStatePart st "occupied_cols"
            -- print cols
            let togg = getStatePart st "toggles"
            -- print togg
            -- putStrLn "test1"
            let result = object ["occupied_rows" .= rows, "occupied_cols" .= cols, "toggles" .= togg]
            -- print result
            putStrLn $ render st
            let responseBody = YAML.encode result
            respond $ responseLBS status200 [(hContentType, "application/x-yaml")] (BSL.fromStrict responseBody)
          else do
            if urlContainsToggle (map T.unpack(pathInfo request)) token
            then do
              putStrLn "Don't tell people you toggle frequently..."
              let st = getState token
              -- print st
              let newSt = toggle st $ T.unpack $ getUrlToggle $ pathInfo request
              print newSt
              addGameSt token newSt
              let st = getState token
              -- print st
              let result = object ["response" .= ("True" :: String)]
              let responseBody = YAML.encode result
              respond $ responseLBS status200 [(hContentType, "application/x-yaml")] (BSL.fromStrict responseBody)
            else do
              putStrLn "New user, old game!"
              let result = object ["game_setup_id" .= ("3a7a8f44-b224-40ff-9c5c-58a1b60eab4b" :: String),
                                  "occupied_rows" .= ([1, 1, 2, 3, 1, 4, 2, 4, 2, 0] :: [Int]),
                                  "occupied_cols" .= ([2, 0, 2, 2, 2, 0, 6, 0, 3, 3] :: [Int])]
              let responseBody = YAML.encode result
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

addGame :: Token -> IO ()
addGame token = do
  g <- readIORef games
  writeIORef games (Map.insert token (State [("occupied_rows", "1123142420"), ("occupied_cols", "2022206033"), ("toggles", "")]) g)

addGameSt :: Token -> State -> IO ()
addGameSt token st = do
  g <- readIORef games
  writeIORef games (Map.insert token st g)

findGame :: Token -> IO (Maybe State)
findGame token = do
  g <- readIORef games
  return (Map.lookup token g)

-- gameExists :: Token -> Bool
-- gameExists token = do
--   g <- findGame token
--   case g of
--     Just g -> True
--     Nothing -> False
gameExists :: Token -> Bool
gameExists token = unsafePerformIO (findGame token Data.Functor.<&> isJust)

urlContainsToggle :: [String] -> Token -> Bool
urlContainsToggle (request : info : xs) token =
  case request of
    "toggle" -> True
    [] -> False
    _ -> urlContainsToggle (info : xs) token
urlContainsToggle (request : xs) token = False
urlContainsToggle [request] _ = False
urlContainsToggle [] _ = False

urlContainsShow :: [String] -> Token -> Bool
urlContainsShow (request : xs) token =
  case request of
    "show" -> True
    [] -> False
    _  -> urlContainsShow xs token
urlContainsShow [request] _ = request == "show"
urlContainsShow [] _ = False


getToken :: Request -> Token
getToken request = T.unpack $ getTokenRecursive $ pathInfo request

getTokenRecursive :: [T.Text] -> T.Text
getTokenRecursive (name : info : xs) =
  case name of
    "game" -> info
    _ -> getTokenRecursive (info : xs)
getTokenRecursive (info : xs) = ""
getTokenRecursive info = ""
getTokenRecursive [] = ""

getState :: Token -> State
getState token = do
  let m = unsafePerformIO $ findGame token
  case m of
    Just state -> state
    Nothing -> error "State not available, I guess..."

getStatePart :: State -> String -> String
getStatePart (State ((name, info) : xs)) str =
  if name == str
    then info
    else getStatePart (State xs) str
getStatePart (State [(name, info)]) str =
  if name == str
    then info
    else error "Cannot find " ++ str
getStatePart (State []) str = error "Cannot find " ++ str

getUrlToggle :: [T.Text] -> T.Text
getUrlToggle (name : info : xs) =
  case name of
    "toggle" -> info
    _ -> getUrlToggle (info : xs)
getUrlToggle (_ : _) = "test1"
getUrlToggle _ = "test2"
