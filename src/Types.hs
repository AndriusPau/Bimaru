{-# LANGUAGE DeriveGeneric #-}
module Types (
    Document(..), Check(..), Coord(..), GameStart(..), Hint(..),
    ToDocument, toDocument,
    FromDocument, fromDocument
) where

import qualified Data.Aeson as A
import Data.Yaml as Y
import Data.HashMap.Strict as HMS
import Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Scientific as S
import GHC.Generics
import Data.String.Conversions

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Gen

-- Data structure used to post ship allocations
-- to game server (for check). Do not modify.
newtype Check = Check {
    coords :: [Coord]
} deriving (Generic, Show, Eq)
instance ToJSON Check

-- Data structure used to post ship allocations
-- to game server (for check). Do not modify.
data Coord = Coord {
    col :: Int,
    row :: Int
} deriving (Generic, Show, Eq)
instance ToJSON Coord

-- Document represents a document which is used to
-- communicate with a game server
data Document =
    DMap [(String, Document)]
    | DList [Document]
    | DInteger Int
    | DString String
    | DNull
    deriving (Show, Ord)

instance Eq Document where
    DNull == DNull = True
    DString s1 == DString s2 = s1 == s2
    DInteger i1 == DInteger i2 = i1 == i2
    DList l1 == DList l2 = l1 == l2
    DMap kv1 == DMap kv2 = L.sort kv1 == L.sort kv2
    _ == _ = False

instance FromJSON Document where
    parseJSON Y.Null = pure DNull
    parseJSON (Y.String t) = pure $ DString (T.unpack t)
    parseJSON (Y.Array v) = DList <$> traverse parseJSON (V.toList v)
    parseJSON (Y.Object o) = DMap <$> traverse (\(k, v) -> parseJSON v >>= (\mv -> pure (T.unpack k, mv))) (HMS.toList o)
    parseJSON (Y.Number s) =
        case S.toBoundedInteger s of
            Nothing -> error $ show s ++ " not an integer"
            Just i -> pure $ DInteger i
    parseJSON a = error $ show a ++ " not supported"

instance ToJSON Document where
    toJSON DNull = A.Null
    toJSON (DString s) = A.String (cs s)
    toJSON (DInteger i) = A.Number (S.scientific (toInteger i) 0)
    toJSON (DList l) = A.Array $ V.fromList $ L.map toJSON l
    toJSON (DMap kvs) = A.Object $ HMS.fromList $ L.map (\(k,v) -> (cs k, toJSON v)) kvs

instance Arbitrary Document where
  arbitrary = arbitraryDocument

arbitraryDocument :: Gen Document
arbitraryDocument = Gen.oneof [arbitraryDString, arbitraryDInteger, arbitraryDList, arbitraryDMap]

arbitraryDString :: Gen Document
arbitraryDString = do
    s <- getSize
    n <- choose (0, min 16 s)
    DString <$> vectorOf n (oneof [arbitraryUpper, arbitraryLower, arbitraryDigit, return ' '])

arbitraryUpper :: Gen Char
arbitraryUpper = chooseEnum ('A', 'Z')

arbitraryLower :: Gen Char
arbitraryLower = chooseEnum ('a', 'z')

arbitraryDigit :: Gen Char
arbitraryDigit = chooseEnum ('0', '9')

arbitraryDInteger :: Gen Document
arbitraryDInteger = DInteger <$> arbitrary

arbitraryDList :: Gen Document
arbitraryDList = do
    s <- getSize
    n <- choose (0, min 4 s)
    DList <$> vectorOf n arbitraryDocument

arbitraryDMap :: Gen Document
arbitraryDMap = do
    s <- getSize
    n <- choose (0, min 4 s)
    DMap <$> vectorOf n ((,) <$> arbitraryK <*> arbitraryDocument)
    where
        arbitraryK = do
            s <- getSize
            n <- choose (1, min 10 s)
            vectorOf n (oneof [arbitraryUpper, arbitraryLower])




-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
newtype GameStart = GameStart Document
    deriving Show

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
newtype Hint = Hint Document 
    deriving Show

class ToDocument a where
    toDocument :: a -> Document

instance ToDocument Check where
    toDocument (Check t) = toDocumentRecursive t (DMap [("coords", DList [])])

toDocumentRecursive :: [Coord] -> Document -> Document
toDocumentRecursive (((Coord c r)) : xs) (DMap [(str, DList l)]) =
  toDocumentRecursive xs (DMap [(str, DList (l ++ [DMap [("col", DInteger c), ("row", DInteger r)  ]  ]  )  )]  )
toDocumentRecursive _ doc = doc

class FromDocument a where
    fromDocument :: Document -> Either String a

instance FromDocument GameStart where
    fromDocument doc 
        | not (existsStateInfo doc "game_setup_id") = Left "Game setup Id not found."
        | not (existsStateInfo doc "occupied_cols") = Left "Occupied collumns information is missing."
        | not (existsStateInfo doc "occupied_rows") = Left "Occupied rows information is missing."
        | not (existsStateInfo doc "number_of_hints") = Left "Hint information is missing."
        | otherwise = Right (GameStart doc)

existsStateInfo :: Document -> String -> Bool
existsStateInfo (DMap ((s, _) : xs)) str =
  s == str || existsStateInfo (DMap xs) str
existsStateInfo _ _ = False

instance FromDocument Hint where
    fromDocument doc 
        | not (existsHintInfo doc "coords") = Left "No coordinate info found."
        | not (checkIfCorrectCoordInfo doc) = Left "Incorrect coordinate info."
        | otherwise = Right (Hint doc)

existsHintInfo :: Document -> String -> Bool
existsHintInfo (DMap ((s, d) : _)) str
  | s == str = True
  | otherwise = existsHintInfo d str
existsHintInfo _ _ = False

--Recursively checks if each col has a coresponding row
checkIfCorrectCoordInfo :: Document -> Bool
checkIfCorrectCoordInfo (DMap ((s, d) : _))
  | s == "coords" && d == DNull = True
  | s == "coords" = checkIfCorrectCoordInfo' d
  | otherwise = checkIfCorrectCoordInfo d
checkIfCorrectCoordInfo _ = False

checkIfCorrectCoordInfo' :: Document -> Bool
checkIfCorrectCoordInfo' (DMap((s1, d1) : (s2, d2) : _))
  | s2 == "tail" && d2 == DNull = True
  | s1 /= "head" && s2 == "tail" = False
  | s1 == "head" && s2 /= "tail" = False
  | s1 == "head" && s2 == "tail" && checkIfCorrectCoordInfo'' d1 = checkIfCorrectCoordInfo' d2 
  | otherwise = False
checkIfCorrectCoordInfo' (DMap((s1, d1) : _))
 | s1 == "tail" && d1 == DNull = True
checkIfCorrectCoordInfo' _ = False

checkIfCorrectCoordInfo'' :: Document -> Bool
checkIfCorrectCoordInfo'' (DMap ((s1, d1) : (s2, d2) : _))
  | s1 == "col" && s2 == "row" = checkIfCorrectCoordInfo''' d1 && checkIfCorrectCoordInfo''' d2
  | otherwise = False
checkIfCorrectCoordInfo'' _ = False

checkIfCorrectCoordInfo''' :: Document -> Bool
checkIfCorrectCoordInfo''' (DInteger _) = True
checkIfCorrectCoordInfo''' _ = False