{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONSGHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib4 () where

import Types ( Document(..), GameStart(..), Hint(..))
--import Lib1 (State(..), emptyState)
import Control.Applicative
import Data.Char
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except (runExceptT)
import Data.List (intercalate)
import Data.List
import Data.Maybe

newtype Parser a = Parser
  { runParser :: ExceptT ParseError (State String) a
  }

newtype ParseError = ParseError String deriving Show



instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ do
    x <- p
    runParser (f x)



instance Semigroup ParseError where
  (ParseError s1) <> (ParseError s2) = ParseError $ concat [s2, " (before failed with: ", s2, ")"]



instance Monoid ParseError where
  mempty = ParseError ""



instance Functor Parser where
  fmap f (Parser p) = Parser $ do
    x <- p
    return $ f x



instance Applicative Parser where
  pure x = Parser $ return x
  (Parser p1) <*> (Parser p2) = Parser $ do
    f <- p1
    a <- p2
    return $ f a



instance Alternative Parser where
  empty = Parser $ throwError $ ParseError "empty"
  (Parser p1) <|> (Parser p2) = Parser $ do
    catchError p1 $ \_ -> p2


instance MonadError ParseError Parser where
  throwError = Parser . throwError
  catchError (Parser p) f = Parser $ catchError p (runParser . f)

-- instance MonadState ParseError Parser where
--   get = lift get
--   put = lift . put


parseDocument :: String -> Either ParseError Document
parseDocument yaml =
  case runState (runExceptT (runParser (parseDocument' 0))) yaml of
  (Right doc, rest) -> do
    if null rest
    then Right doc
    else Left $ ParseError $ "parseDocument: not all input consumed; rest: " ++ rest
  (Left err, _) -> do
    Left err

    -- (Right doc, rest) ->
    --   if null rest
    --   then Right doc
    --   else Left $ ParseError $ "parseDocument: not all input consumed; rest: " ++ rest

parseDocument' :: Int -> Parser Document
--parseDocument' prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList
parseDocument' prevIndent = do
  input <- Parser get
  parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList  prevIndent <|> parseDMap prevIndent <|> parseDString

parseDocumentInList :: Int -> Parser Document
parseDocumentInList prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList (prevIndent+1) <|> parseDMap (prevIndent+1) <|> parseDString

parseDocumentInDMap :: Int -> Parser Document
parseDocumentInDMap prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList prevIndent <|> parseDMap (prevIndent+1) <|> parseDString


-- parseDocument' :: Int -> Parser Document
-- parseDocument' prevIndent = parseDInteger <|> parseDNull


parseChar :: Char -> Parser Char
parseChar c = Parser $ do
  input <- get
  case input of
    (x : xs)
      | x == c -> 
        do
          put xs
          return x
      | otherwise
      -> throwError $ ParseError ("Expected '" ++ [c] ++ "'," ++ " but got '" ++ [x] ++ "'")
    [] -> throwError $ ParseError ("Expected '" ++ [c] ++ "', but got empty string")

parseStr :: String -> Parser String
parseStr str = Parser $ do
  input <- get
  result <- foldM (\acc c -> runParser (parseChar c) >>= \parsedChar -> return (parsedChar:acc)) [] str `catchError` \e -> do
    put input
    throwError e
  return $ reverse result

parseDNull :: Parser Document
parseDNull = Parser $ do
  input <- get
  result <- runParser (parseStr "null\n") `catchError` \e -> do
    put input
    throwError e
  return $ DNull

spanParserMany :: (Char -> Bool) -> Parser String
spanParserMany f = Parser $ do
  str <- get
  let (xs, ys) = span f str
  put ys
  return xs

spanParserSome :: (Char -> Bool) -> Parser String
spanParserSome f = Parser $ do
  str <- get
  let (xs, ys) = span f str
  if null xs
  then do
    throwError $ ParseError "No parsers matched"
  else do
    put ys
    return xs

parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ do
  input <- get
  case input of
    (y:ys)
      | f y -> do
        put ys
        return y
    [] -> throwError $ ParseError "Expected a character that satisfies the given predicate, but reached the end of the string"
    _ -> throwError $ ParseError "Expected a character that satisfies the given predicate, but got something else"

parseIf' :: (Char -> Bool) -> Parser Char
parseIf' f = Parser $ do
  input <- get
  case input of
    (y : ys)
      | f y -> 
        do
          put (y:ys)
          return ' '
      | otherwise
        -> throwError $ ParseError ("Expected a character, but got '" ++ [y] ++ "'")
    [] -> throwError $ ParseError $ "Expected a character, but reached end of string"

intLiteral :: Parser Int
intLiteral = (parseChar '-' *> fmap negate nonNegativeLiteral) <|> nonNegativeLiteral

nonNegativeLiteral :: Parser Int
nonNegativeLiteral = read <$> spanParserSome isDigit

parseDInteger :: Parser Document
parseDInteger = Parser $ do
  input <- get
  runParser ( DInteger <$> (optional (spanParserMany isSpace) *> intLiteral <* parseChar '\n')) `catchError` \e -> do
    put input
    throwError e

stringLiteral :: Parser String
stringLiteral = Parser $ do
  input <- get
  runParser ( spanParserSome isAlphaNumSpace) `catchError` \e -> do
    put input
    throwError e

parseDString :: Parser Document
parseDString = DString <$> (optional (spanParserMany isSpace) *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"') <* parseChar '\n')

parseDMap :: Int -> Parser Document    --Galimai blogai
parseDMap prevIndent = Parser $ do
  input <- get
  let indentation = (countSpaces input `div` 2)
  if prevIndent == indentation then runParser $ ( DMap <$> some (parsePair prevIndent))
    else if prevIndent < indentation
      then runParser $ (parseDocument' prevIndent)
    else throwError $ ParseError $ "errorDMAP"
{-
key:
- 1
- 2
  map:
  - 3
  - 4
DMap[(key, DList[1, 2])]


DList[DMap[(key, 123)]]

-}
parsePair :: Int -> Parser (String, Document)
parsePair prevIndent = Parser $ do
  input <- get
  let indentation = (countSpaces input `div` 2 )
  if prevIndent == indentation then do 
    key <- (runParser $ (optional (spanParserMany isSpace) *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"') <* (parseChar ':'))) `catchError` \e -> do
      put input
      throwError e
    _ <- runParser $ optional (parseChar '\n')
    value <- runParser $ parseDocumentInDMap prevIndent
    return (key, value)
  else throwError $ ParseError $ "error DPair"

parseDList :: Int -> Parser Document
parseDList prevIndent = Parser $ do
  input <- get
  let indentation = countSpaces input `div` 2 in
    if prevIndent == indentation then runParser (DList <$> some ( optional (parseChar '\n') *> removeSpaces prevIndent *> parseStr "- " *> addSpaces (prevIndent+1) *> parseListItem prevIndent))
    else throwError $ ParseError "errorDLIST"

parseListItem :: Int -> Parser Document
parseListItem prevIndent = Parser $ do
  input <- get
  let indentation = countSpaces input `div` 2 - 1 in
    if prevIndent == indentation then runParser ( optional (parseChar '\n') *> parseDocumentInList prevIndent)
    else throwError $ ParseError "errorDLISTITEM"

parseEmptyDList :: Parser Document
parseEmptyDList = Parser $ do
  input <- get
  result <- runParser (spanParserMany isSpace *> parseStr "[]" <* parseChar '\n') `catchError` \e -> do
    put input
    throwError e
  return $ DList []

parseEmptyDMap :: Parser Document
parseEmptyDMap = Parser $ do
  input <- get
  result <- runParser (spanParserMany isSpace *> parseStr "{}" <* parseChar '\n') `catchError` \e -> do
    put input
    throwError e
  return $ DMap []

parseEmptyDStringDoubleQuotes :: Parser Document
parseEmptyDStringDoubleQuotes = Parser $ do
  input <- get
  result <- runParser (spanParserMany isSpace *> parseStr "\"\"" <* parseChar '\n') `catchError` \e -> do
    put input
    throwError e
  return $ DString []

parseEmptyDStringSingleQuotes :: Parser Document
parseEmptyDStringSingleQuotes = Parser $ do
  input <- get
  result <- runParser (spanParserMany isSpace *> parseStr "\'\'" <* parseChar '\n') `catchError` \e -> do
    put input
    throwError e
  return $ DString []

-- HELPER FUNCTIONS

--This checks if the given character is an alphabetic character, a digit or a space
isAlphaNumSpace :: Char -> Bool
isAlphaNumSpace c = isAlphaNum c || c == ' ' || c == '_' || c == '-'

--This removes n spaces from the beginning of the input string
--Returns Right Parser String the removed spaces and the rest of the input string: String -> (String, String)
--Returns Left String with error message on failure
removeSpaces :: Int -> Parser String
removeSpaces n = Parser $ do
  input <- get
  let indentation = countSpaces input `div` 2 in
    if n <= indentation then do
      put (drop (n*2) input)
      return (take (n*2) input)
    else throwError $ ParseError "Error: Indentation is not correct"

--This adds n spaces to the beginning of the input string
--Returns Right Parser String holding an empty character and spaces with input string: String -> (String, String)
--Returns Left String with error message on failure
addSpaces :: Int -> Parser String
addSpaces n = Parser $ do
  input <- get
  case addSpace "" n of
    Right spaces -> do
      put (spaces ++ input)
      return (spaces ++ input)
    Left err -> throwError $ ParseError err

--This recursively adds two spaces to the beginning of the input string
--Returns Right (String, String) holding an empty character and spaces with input string
--Returns Left String with error message on failure
addSpace :: String -> Int -> Either String String
addSpace acc 0 = Right acc
addSpace acc n = addSpace ("  " ++ acc) (n-1)

--This counts the number of spaces at the beginning of the input string
--Returns the number of spaces
countSpaces :: String -> Int
countSpaces str =
  let (a, _) = span isSpaceOrNewLine str in
    if null a
      then 0
    else
      if head a == '\n'
        then length a - 1
      else length a

--This checks if the given character is a space or a new line
isSpaceOrNewLine :: Char -> Bool
isSpaceOrNewLine c = isSpace c || c == '\n'
