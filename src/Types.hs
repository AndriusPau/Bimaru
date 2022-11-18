{-# LANGUAGE DeriveGeneric #-}
module Types (
    Document(..), Check(..), Coord(..),
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
arbitraryDString =
    DString <$> listOf (chooseEnum ('\32', '\126'))

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
        arbitraryK = listOf1 (chooseEnum ('\32', '\126'))

class ToDocument a where
    toDocument :: a -> Document

class FromDocument a where
    fromDocument :: Document -> Either String a
