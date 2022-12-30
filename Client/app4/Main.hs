{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify, put, StateT )
import Data.ByteString as B ( empty, ByteString )
import Data.ByteString.Char8 as BS (unpack)
-- import Data.Either as E (fromRight)
import qualified Data.List as L
-- import Data.Text as T ( concat, drop, pack, unpack, Text )
import Data.Text as T ( concat, pack, unpack, Text )
import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Data.Char (isSpace)
-- import Lib4 ( emptyState, mkCheck, render, toggle, State )
-- import Lib2 ( renderDocument )
-- import Lib4 ( parseDocument, hint, gameStart, Hint, GameStart)
import Lib4 (emptyState, State (..), Document (..), gameStart, GameStart, parseDocument, render, renderDocument)
import Types(Check, toDocument, fromDocument)
import Network.Wreq
    ( post, postWith, defaults, header, responseBody )
-- import qualified Network.Wreq as Wreq

import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Data.String.Conversions
import GHC.IO (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)

type Repl a = HaskelineT (StateT String IO) a

{-# NOINLINE solved #-}
solved :: IORef String
solved = unsafePerformIO (newIORef "You're a failure, Harry!")

solvedTrue :: IO ()
solvedTrue = do
  Prelude.putStrLn "Set to True"
  writeIORef solved "You did it!"

solvedFalse :: IO ()
solvedFalse = do
  Prelude.putStrLn "Set to False"
  writeIORef solved "You're a failure, Harry!"

getSolved :: IO String
getSolved = readIORef solved

commandShow :: String
commandShow = "show"

-- commandHint :: String
-- commandHint = "hint"

commandCheck :: String
commandCheck = "check"

commandToggle :: String
commandToggle = "toggle"

commandExit :: String
commandExit = "exit"

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = do
    str <- Main.show
    -- liftIO $ Prelude.putStrLn str
    let gs =  Lib4.parseDocument str
    case gs of
      Left err -> liftIO $ Prelude.putStrLn err
      Right info -> do
        let st = docToState info emptyState
        liftIO $ Prelude.putStrLn $ Lib4.render st
  | trim c == commandCheck = do
    sol <- liftIO getSolved
    if sol == "You did it!" then liftIO $ exit $ cs (T.pack ("You won! Now get out!" :: String)) else liftIO . Prelude.putStrLn $ sol
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " expects at least one argument"
      t -> do
        str <- toggle $ tokens c
        -- liftIO $ print str
        if str == "response: 'True'\n"
          then liftIO solvedTrue
          else liftIO solvedFalse
  -- "123455" => ["toggle", "12", "34", "55"]

  -- | trim c == commandShow = lift get >>= liftIO . Prelude.putStrLn . Lib1.render . snd
  -- | trim c == commandCheck = lift get >>= check . (Lib1.mkCheck . snd) >>= liftIO . Prelude.putStrLn
  -- | commandToggle `L.isPrefixOf` trim c = do
  --   case tokens c of
  --     [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " expects at least one argument"
  --     t -> lift $ modify (\(u, s) -> (u, Lib1.toggle s (L.drop 1 t)))

  | trim c == commandExit = liftIO $ exit $ cs (T.pack ("Giving up already?" :: String))
  -- | trim c == commandExit = break (const True) ""
-- | commandHint `L.isPrefixOf` trim c =
  --   case tokens c of
  --     [_, str] ->
  --       case reads str of
  --         [(n, "")] -> hints n
  --         _ -> liftIO $ Prelude.putStrLn $ "Illegal " ++ commandHint ++ " argument: " ++ str
  --     _ -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandHint ++ " $number_of_hints\" expected, e.g \"" ++ commandHint ++ " 1\""
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

check :: Check -> Repl String
check c = do
  url <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ renderDocument $ toDocument c :: B.ByteString
  resp <- liftIO $ postWith opts (url ++ "/check") body
  pure $ cs $ resp ^. responseBody

toggle :: [String] -> Repl String
toggle xs = do
  url <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ Prelude.concat $ tail xs :: B.ByteString
  resp <- liftIO $ postWith opts (url ++ "/toggle/" ++ BS.unpack body) body
  pure $ cs $ resp ^. responseBody

show :: Repl String
show = do
  url <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = "test" :: B.ByteString
  resp <- liftIO $ postWith opts (url ++ "/show") body
  pure $ cs $ resp ^. responseBody

docToState :: Document -> State -> State
docToState (DMap ((name, info) : xs)) (State st)
  | name == "toggles" = docToState (DMap xs) (State ((name, info) : st))
  | name == "occupied_rows" = docToState (DMap xs) (State ((name, info) : st))
  | name == "occupied_cols" = docToState (DMap xs) (State ((name, info) : st))
docToState _ st = st

  -- Return Bool from server maybe????? send help
  --let result = cs resp ^. responseBody
  --case result of
  --  "true" -> pure True
  --  "false" -> pure False
  --  _ -> throwError $ "Server error " ++ result

-- hints :: Int -> Repl ()
-- hints n = do
--   (url, s) <- lift get
--   r <- liftIO $ Wreq.get (url ++ "/hint?limit=" ++ show n)
--   let h = Lib4.parseDocument (cs (r ^. responseBody)) >>= fromDocument
--   case (h :: Either String Lib4.Hint) of
--     Left msg -> liftIO $ fatal $ cs msg
--     Right d -> lift $ put (url, Lib4.hint s d)

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  -- let names = [commandShow, commandHint, commandCheck, commandToggle]
  let names = [commandShow, commandCheck, commandToggle, commandExit]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  url <- lift get
  r <- liftIO $ post url B.empty
  let gs = Lib4.parseDocument (cs (r ^. responseBody)) >>= fromDocument
  case (gs :: Either String Lib4.GameStart) of
    Left msg -> liftIO $ fatal $ cs msg
    Right d -> do
      str <- toggle ["toggle", "1111"]
      if str == "response: 'True'\n"
        then liftIO solvedTrue
        else liftIO solvedFalse
      lift $ put url
      liftIO $ TIO.putStrLn "Welcome to Bimaru v4. Press [TAB] for available commands list"

fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

exit :: Text -> IO ()
exit msg = do
  TIO.hPutStrLn stderr msg
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> run $ T.pack token
    _ -> fatal "token not provided, expected at least one command argument"

run :: T.Text -> IO ()
run token = do
  -- Dear students, it is not against you, it is against silly source code crawlers on the Internet
  -- let url = case decodeBase64 "YmltYXJ1LmhvbWVkaXIuZXU=" of
  --          Right x -> x
  --          Left _  -> error "Cannot decode url"
  -- let url = E.fromRight (error "Cannot decode url") $ decodeBase64 $ T.drop 6 "f6675cYmltYXJ1LmhvbWVkaXIuZXU="

  -- let fullUrl = T.unpack (T.concat ["http://", url, "/game/", token])
  let fullUrl = T.unpack (T.concat ["http://", "localhost:8080", "/game/", token])
  evalStateT (evalRepl (const $ pure "==> ") cmd [] Nothing Nothing (Word completer) ini final) fullUrl

-- "game_setup_id: 3a7a8f44-b224-40ff-9c5c-58a1b60eab4b
-- \nnumber_of_hints: 10
-- \noccupied_rows:\n- 1\n- 1\n- 2\n- 3\n- 1\n- 4\n- 2\n- 4\n- 2\n- 0
-- \noccupied_cols:\n- 2\n- 0\n- 2\n- 2\n- 2\n- 0\n- 6\n- 0\n- 3\n- 3\n"
