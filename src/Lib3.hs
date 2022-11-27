{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint
    ) where

import Data.String.Conversions

import Data.Yaml as Y (encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)


import Types ( Document(..), GameStart(..), Hint(..))
import Lib1 (State(..), emptyState)
import Lib2 (renderDocument)
import Control.Applicative
import Data.Char

stringTest :: String
stringTest = "l:\n  MYb:\n  - ' 4k'\n  - 'Hj '\n  - s: []\n    A:\n    - - - -3\n        - 'fi   '\n      - 'Y'\n      - {}\n      - ' t'\n    - 'u 71 '\n    - - 5\n    - - -5\n      - AuxxI:\n        - mTmMJ:\n          - 1\n          - V: -2\n            nntE: 4\n            D:\n            - []\n            - t\n            - -1\n            fc:\n            - rx:\n              - 3\n              Q: -2\n            - 5\n            - c4Vj\n            - '0 '\n          - 1\n          - 3\n          q:\n          - ''\n          - hFI: hq0\n            Xl:\n            - -2\n            - I\n            LGG:\n            - kKirH: 1\n              FTmSj:\n                njX: []\n                Df:\n                - {}\n                j:\n                - -5\n                - 1\n                - VfmD:\n                    HQE: 2\n                    aMA:\n                      hD:\n                        k:\n                          Fk: '71'\n                        lbY:\n                        - '79I5 '\n                        - 4\n                      Wre: 0\n                  Ta: 1\n                - uQ: -3\n                  r: []\n                  Js: {}\n                QDFD:\n                  Ou: 2\n                  s: 3\n                  cgSN: -1\n            JyyyN: {}\n          - 5\n          - r8\n      - 4 9u\nXk: []\nxTJE:\n  q: vns\n  ZvODt:\n  - oy: I\n    oS: 5\n  - IqpS:\n      xG: 0\n      M: []\n      ZFET:\n      - ayL: e  u\n        XGZu:\n          nq:\n            j: 'Ss F '\n            DnS: k5\n        QQz: 3\n      - ''\n      uqu:\n        XaecS: []\n        m:\n          oa:\n          - - {}\n          - 'w '\n          - -5\n          - []\n          fZf: 2\n          eR:\n          - - -3\n            - uA:\n                HWOU: 2\n                air: 0\n                P: -1\n              lpSG: -3\n          - Vop: Tb7j\n            qRSz:\n              EST: -3\n            abzD:\n            - - - - -4\n                - ''\n                - e: 2\n                  GURFO: l V\n                  Ouv: 1\n                - Bw:\n                  - -2\n                  - ''\n                  - 'H   '\n              - 3\n            - 'bP8 '\n            - {}\n            F: ' z'\n    GxSj: -2\n  WoY: -1\nYUYL: {}\n"

stringTest' :: String
stringTest' = "bDX:\n  IAGq: 1\n  SrmO: \"5\"\ngIzz: \"G  h6\"\niA- []\n"

docTest :: String
docTest = renderDocument (DMap [("bDX",DMap [("IAGq",DInteger 1),("SrmO",DString "5")]),("gIzz",DString "G  h6"),("iA",DList [])])

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)


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
    Right $ (f a, rest')

instance Alternative Parser where
  empty = Parser $ const $ Left "empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Left _ -> p2 input
      x      -> x

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument yaml = do
    (a,b) <- runParser (parseDocument' 0) yaml
    case (a,b) of
      (DNull, "") -> Right DNull
      (DNull, _) -> Left "error"
      (doc, "") -> Right doc
      (doc, b) -> Left (show doc ++ "||||||||||||||||||||||||||||||||||||" ++ b)


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
      [] -> Left $ "Expected a character, but reached end of string"

parseIf' :: (Char -> Bool) -> Parser Char
parseIf' f =
  Parser $ \input ->
    case input of
      y:ys
        | f y       -> Right (' ', y:ys)
        | otherwise -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
      [] -> Left $ "Expected a character, but reached end of string"

intLiteral :: Parser Int
intLiteral = parseChar '-' *> fmap negate nonNegativeLiteral <|> nonNegativeLiteral

nonNegativeLiteral :: Parser Int
nonNegativeLiteral = read <$> spanParserSome isDigit

stringLiteral :: Parser String
stringLiteral = spanParserSome isAlphaNumSpace

parseDNull :: Parser Document
parseDNull = Parser $ \input -> do
  let result = runParser (parseStr "null\n") input in
    case result of
      Left a -> Left a
      _ -> Right (DNull, drop 5 input)

parseDInteger :: Parser Document
parseDInteger = DInteger <$> (optional (spanParserMany isSpace) *> intLiteral <* parseChar '\n')

parseDString :: Parser Document
parseDString = DString <$> (optional (spanParserMany isSpace) *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"') <* parseChar '\n')

parseDMap :: Int -> Parser Document
parseDMap prevIndent = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
  if prevIndent == indentation then runParser ( DMap <$> some (parsePair prevIndent)) input
    else if prevIndent < indentation then runParser (parseDocument' prevIndent) input
    else Left "errorDMAP"

parsePair :: Int -> Parser (String, Document)
parsePair prevIndent = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
    if prevIndent == indentation then runParser ((,) <$> (optional (parseChar '\n') *> removeSpaces prevIndent *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"')) <*> (parseChar ':' *> optional(parseChar ' ') *> optional (parseChar '\n') *> parseDocumentInDMap prevIndent)) input
    else Left "errorDPAIR"

parseDList :: Int -> Parser Document
parseDList prevIndent = Parser $ \input ->
  let indentation = countSpaces input `div` 2 in
    if prevIndent == indentation then runParser (DList <$> some ( optional (parseChar '\n') *> removeSpaces prevIndent *> parseStr "- " *> addSpaces (prevIndent+1) *> parseListItem prevIndent)) input
--    else if prevIndent < indentation then runParser (parseDocument' prevIndent) input
    else Left "errorDLIST"

parseListItem :: Int -> Parser Document
parseListItem prevIndent = Parser $ \input ->
  let indentation = countSpaces input `div` 2 - 1 in
    if prevIndent == indentation then runParser ( optional (parseChar '\n') *> parseDocumentInList prevIndent) input
    else Left "errorDLISTITEM"

parseEmptyDList :: Parser Document
parseEmptyDList = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "[]" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DList [], drop 3 (dropWhile isSpace input))

parseEmptyDMap :: Parser Document
parseEmptyDMap = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "{}" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DMap [], drop 3 (dropWhile isSpace input))

parseEmptyDStringDoubleQuotes :: Parser Document
parseEmptyDStringDoubleQuotes = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "\"\"" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DString "", drop 3 (dropWhile isSpace input))

parseEmptyDStringSingleQuotes :: Parser Document
parseEmptyDStringSingleQuotes = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "''" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DString "", drop 3 (dropWhile isSpace input))

parseDocument' :: Int -> Parser Document
parseDocument' prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList  prevIndent <|> parseDMap prevIndent  <|> parseDString

parseDocumentInList :: Int -> Parser Document
parseDocumentInList prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList (prevIndent+1) <|> parseDMap (prevIndent+1) <|> parseDString

parseDocumentInDMap :: Int -> Parser Document
parseDocumentInDMap prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList prevIndent <|> parseDMap (prevIndent+1) <|> parseDString

-- HELPER FUNCTIONS

isAlphaNumSpace :: Char -> Bool
isAlphaNumSpace c = isAlphaNum c || c == ' '

removeSpaces :: Int -> Parser String
removeSpaces n = Parser $ \input ->
  let indentation = countSpaces input `div` 2 in
    if n <= indentation then Right ("", drop (n*2) input)
    else Left "error"

addSpaces :: Int -> Parser String
addSpaces n = Parser $ \input -> addSpace input n

addSpace :: String -> Int -> Either String (String, String)
addSpace acc 0 = Right ("", acc)
addSpace acc n = addSpace ("  " ++ acc) (n-1)

countSpaces :: String -> Int
countSpaces str =
  let (a, _) = span isSpaceOrNewLine str in
    if null a
      then 0
    else
      if head a == '\n'
        then length a - 1
      else length a

isSpaceOrNewLine :: Char -> Bool
isSpaceOrNewLine c = isSpace c || c == '\n'

removeStartingDashes :: String -> String
removeStartingDashes str =
  let (a, b) = span (== '-') str in
    if null a
      then str
    else
      if head b == '\n'
        then drop 1 b
    else str