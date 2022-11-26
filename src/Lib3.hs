{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument', GameStart, Hint
    ) where

import Types ( Document(..), GameStart(..), Hint(..))
import Lib1 (State(..), emptyState)
import Control.Applicative
import Data.Char


{-

DList
parseDocument "" gives DMap[] or DList[]
String with no char after it gives wrong doc

runParser (parseDocumentWID 0) "- labas\n- pasauli\n- - key1:\n      key11: 11"


-}

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument yaml = do
  (a,b) <- runParser (parseDocumentWID 0) yaml
  case (a,b) of
    (DNull, "") -> Right DNull
    (DNull, _) -> Left "Incorrect yaml"
    (doc, "") -> Right doc
    (doc, _) -> Right doc


-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State l) (GameStart doc) = gameStartRecursive (State l) doc

gameStartRecursive :: State -> Document -> State
gameStartRecursive (State l) (DMap ((s, d) : xs)) =
  case s of
    "game_setup_id" -> State (("toggles", DString []) : ("hints", DString []) : ((s, d) : l))
    "occupied_rows" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "occupied_cols" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "number_of_hints" -> gameStartRecursive (State ((s, d) : l)) (DMap xs)
    _ -> gameStartRecursive (State l) (DMap xs)
gameStartRecursive _ _ = emptyState

-- This gets a whole string of the gamestate information, which is shown at the top and the left side of the board.
-- Meant to be used by drawGridTop, getSingleDIntValue functions and uses the getDIntValue (recursively) function.
-- String - The document string, that needs to be parsed.
-- String - A temporary recursive string that keeps track of what we are reading. (Use [] when calling this function)
-- String - A temporary recursive string that keeps track of what result we have collected. (Use [] when calling this function)
-- String - The result string that is filled with chained (no whitespace characters) side information values.
getDIntValue :: String -> String -> String -> String
getDIntValue (x : xs) str rez =
  if x /= ']'
    then
      if length str == 9
        then
          if str == "DInteger "
            then getDIntValue xs [] (rez ++ [x])
            else getDIntValue xs (tail str ++ [x]) rez
        else getDIntValue xs (str ++ [x]) rez
    else rez
getDIntValue _ _ _ = "test"

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State l) (Hint doc) = hints (State l) doc

hints :: State -> Document -> State
hints (State l) (DMap ((_, d) : _)) = hintState (State l) (State []) d
hints _ _ = emptyState

hintState :: State -> State -> Document -> State
hintState (State ((st, doc) : xs)) (State temp) d =
  if st == "hints"
    then State (temp ++ (("hints", setHint d) : xs))
    else hintState (State xs) (State (temp ++ [(st, doc)])) d
hintState _ _ _ = emptyState

setHint :: Document -> Document
setHint doc = DString (getHintsString (show doc) [] [])

getHintsString :: String -> String -> String -> String
getHintsString (x : xs) str rez
  | x /= 'N' =
    if length str == 9
      then
        if str == "DInteger "
          then getHintsString xs [] (rez ++ [x])
          else getHintsString xs (tail str ++ [x]) rez
      else getHintsString xs (str ++ [x]) rez
  | null rez = ""
  | otherwise = rez
getHintsString _ _ _ = ""




newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, rest) <- p1 input
    (a, rest') <- p2 rest
    Right (f a, rest')

instance Alternative Parser where
  empty = Parser $ const $ Left "empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Left _ -> p2 input
      x      -> x

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
  case input of
    (x : xs) | x == c -> Right (c, xs)
             | otherwise -> Left $ "Expected '" ++ [c] ++ "'," ++ " but got '" ++ [x] ++ "'"
    [] -> Left $ "Expected '" ++ [c] ++ "', but got empty string"

parseStr :: String -> Parser String
parseStr str = Parser $ \input ->
  let result = runParser (traverse parseChar str) input in
  case result of
    Left _ -> Left $ "Expected \"" ++ str ++ "\", but got \"" ++ input ++ "\""
    _ -> result

spanParserMany :: (Char -> Bool) -> Parser String
spanParserMany f = many $ parseIf f

spanParserSome :: (Char -> Bool) -> Parser String
spanParserSome f = some $ parseIf f

parseIf :: (Char -> Bool) -> Parser Char
parseIf f =
  Parser $ \input ->
    case input of
      y:ys
        | f y       -> Right (y, ys)
        | otherwise -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
      [] -> Left "Expected a character, but reached end of string"

parseIf' :: (Char -> Bool) -> Parser Char
parseIf' f =
  Parser $ \input ->
    case input of
      y:ys
        | f y       -> Right (' ', y:ys)
        | otherwise -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
      [] -> Left "Expected a character, but reached end of string"

intLiteral :: Parser Int
intLiteral = parseChar '-' *> fmap negate nonNegativeLiteral <|> nonNegativeLiteral

nonNegativeLiteral :: Parser Int
nonNegativeLiteral = read <$> spanParserSome isDigit

stringLiteral :: Parser String
stringLiteral = spanParserSome isAlphaNum

parseDNull :: Parser Document
parseDNull = Parser $ \input -> do
  let result = runParser (parseStr "null") input in
    case result of
      Left a -> Left a
      _ -> Right (DNull, drop 4 input)

parseDInteger :: Parser Document
parseDInteger = DInteger <$> intLiteral

parseDString :: Parser Document
parseDString = DString <$> ( optional (parseChar '"') *> stringLiteral <* parseIf' (/= ':') <* optional (parseChar '"'))

parseDMap :: Int -> Parser Document
parseDMap depth = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
  if depth >= indentation then runParser ( DMap <$> some (parsePair depth)) input
    else if depth < indentation then runParser (parseDocument' depth) input
    else Left "errorDMAP"

parsePair :: Int -> Parser (String, Document)
parsePair depth = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
    if depth == indentation then runParser ((,) <$> (spanParserMany isSpace *> stringLiteral) <*> (parseChar ':' *> optional(parseChar ' ') *> optional (parseChar '\n') *> parseDocumentWID depth)) input
    else Left "errorDPAIR"

parseEmptyDMap :: Parser Document
parseEmptyDMap = Parser $ \input ->
  let result = runParser (parseStr "{}") input in
    case result of
      Left a -> Left a
      _ -> Right (DMap [], drop 2 input)

parseDList :: Int -> Parser Document
parseDList depth = Parser $ \input ->
  let indentation = countSpacesAndDashes input `div` 2 - 1 in
    if depth == indentation then runParser (DList <$> some (parseListItem depth)) input
    else if depth < indentation then runParser (parseDocument' depth) input
    else Left "errorDLIST"

parseListItem :: Int -> Parser Document
parseListItem depth = Parser $ \input ->
  let indentation = countSpacesAndDashes input `div` 2 - 1 in
    if depth == indentation then runParser ( optional (parseChar '\n') *> optional (spanParserMany isSpace) *> some (parseStr "- ") *> parseDocumentWID depth) input
    else Left "errorDLISTITEM"

parseEmptyDList :: Parser Document
parseEmptyDList = Parser $ \input ->
  let result = runParser (parseStr "[]") input in
    case result of
      Left a -> Left a
      _ -> Right (DList [], drop 2 input)

{-

"
coords:
- col:
  - row: 6

"coords:\n- col:\n  - row: 6"

DMap [("coords",DList [DMap [("col",DList [DMap [("row",DInteger 6)]])]])]

- col: 1
  row: 9
"

"coords:\n- col: 1\n  row: 6\n- col: 1\n  row: 9\n"

coords: 
- row: 2
- col1: 1
- - col2: 2

"coords:\n- row: 2\n- col1: 1\n- - col2: 2"

DMap[("coords",DList[DMap[("row",DInteger 2)],DMap[("col1",DInteger 1)],DList[DMap[("col2",DInteger 2)]]])]

coords: 
- row: 2
  col1: 1
- - col2: 2

"coords:\n- row: 2\n  col1: 1\n- - col2: 2"

DMap[("coords",DList[DMap[("row",DInteger 2),("col1",DInteger 1)],DList[DMap[("col2",DInteger 2)]]])]




-}

countChars :: Parser Int
countChars = length <$> spanParserMany isDashOrSpace

countSpaces :: String -> Int
countSpaces str =
  let (a, b) = span isSpace str in
    length a

countSpacesAndDashes :: String -> Int
countSpacesAndDashes str =
  let (a, b) = span isDashOrSpace str in
    if null a
      then 0
    else
      if head a == '\n'
        then length a - 1
      else length a

isDashOrSpace :: Char -> Bool
isDashOrSpace c = c == '-' || isSpace c || c == '\n'


parseDocument' :: Int -> Parser Document
parseDocument' depth = parseDInteger <|> parseDNull <|> parseDString <|> parseEmptyDMap <|> parseEmptyDList {-<|> parseDList (depth + 1)-} <|> parseDMap (depth+1)

parseDocumentWID :: Int -> Parser Document
parseDocumentWID depth = parseDInteger <|> parseDNull <|> parseDString <|> parseEmptyDMap <|> parseEmptyDList {-<|> parseDList depth-} <|> parseDMap depth

-- parseDocument' :: Int -> Parser Document
-- parseDocument' depth = parseDInteger <|> parseDNull <|> parseDString <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList (depth + 1) <|> parseDMap (depth+1)

-- parseDocumentWID :: Int -> Parser Document
-- parseDocumentWID depth = parseDInteger <|> parseDNull <|> parseDString <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList depth <|> parseDMap depth


parseDocumentInDMAP :: Int -> Parser Document
parseDocumentInDMAP depth = parseDInteger <|> parseDNull <|> parseDString <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList depth <|> parseDMap (depth+1)

