import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (renderDocument, gameStart, hint)
import Lib1 (State (..))
import Types (Document(..))
import Data.Either

main :: IO ()
main = defaultMain (
  testGroup "Tests"
  [
  toYamlTests,
  gameStartTests,
  hintTests
  ]
  )

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "Null" $
        renderDocument DNull @?= "null"

    , testCase "Int" $
        renderDocument (DInteger 789123) @?= "789123"
    , testCase "String" $
        renderDocument (DString "Test123") @?= "Test123"


    , testCase "Empty List" $
        renderDocument (DList []) @?= "[]"
    , testCase "List of ints" $
        renderDocument (DList [DInteger 1, DInteger 2, DInteger 3, DInteger 4, DInteger 5, DInteger 6, DInteger 7, DInteger 111]) @?= "[1, 2, 3, 4, 5, 6, 7, 111]"
    , testCase "List of nulls" $
        renderDocument (DList [DNull, DNull, DNull]) @?= "[null, null, null]"
    , testCase "List of strings" $
        renderDocument (DList [DString "Hello", DString "World", DString "!!"]) @?= "[Hello, World, !!]"
    , testCase "List of DMaps" $
        renderDocument (DList[DMap[("1.1", DInteger 1), ("1.2", DInteger 2)], DMap[("2.1", DInteger 1)], DMap[("3.1", DInteger 1), ("3.2", DInteger 2)]]) @?= "[{1.1: 1, 1.2: 2}, {2.1: 1}, {3.1: 1, 3.2: 2}]"

    , testCase "Empty DMap" $
        renderDocument (DMap[]) @?= "{}"
    , testCase "DMap with one element" $
        renderDocument (DMap[("DNull", DNull)]) @?= "{DNull: null}"
    , testCase "DMap with list of ints inside" $
        renderDocument (DMap [("List", DList[DInteger 1, DInteger 2, DInteger 3, DInteger 4])]) @?= "{List: [1, 2, 3, 4]}"
    , testCase "DMap with a DMap which has a DMap inside. (LOL)" $
        renderDocument (DMap[("first", DMap[("second", DMap[("third", DString "The end")])])]) @?= "{first: {second: {third: The end}}}"

    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document"
  [   testCase "No information missing" $
        show (fromRight (State[]) (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
        ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
        ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")])))
        @?=
        show (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)])

    ,   testCase "Extra information added" $
        show (fromRight (State[]) (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
        ("additional_info1", DString "Additional info"),
        ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
        ("additional_info2", DInteger 420),
        ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
        ("additional_info3", DMap [("additional_info_inside_additional_info", DList [DNull, DInteger 6, DInteger 9])]),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")])))
        @?=
        show (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)])

    , testCase "Missing Collumn info" $
        fromLeft "(State[])" (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
        ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")]))
        @?=
        "Occupied collumns information is missing."

    , testCase "Missing Row info" $
        fromLeft "(State[])" (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
        ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")]))
        @?=
        "Occupied rows information is missing."

    , testCase "Missing Hint info" $
        fromLeft "(State[])" (gameStart (State []) (DMap [
        ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
        ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")]))
        @?=
        "Hint information is missing."

    , testCase "Missing Game Setup Id info" $
        fromLeft "(State[])" (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
        ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
        ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0])]))
        @?=
        "Game setup Id not found."

  ]

hintTests :: TestTree
hintTests = testGroup "Test hint document" 
  [
    testCase "No information missing" $
        show (fromRight (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]) 
        (DMap[("Hints",DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])])
        @?=
        show (State [("toggles",DString ""),
        ("hints",DString "6867"),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]

    , testCase "Missing Hints tag" $
        show (fromRight (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]) 
        (DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])
        @?=
        "No hint info found."
    
    , testCase "Missing corresponding col info" $
        show (fromRight (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]) 
        (DMap[("Hints",DMap[("coords",DMap[("head",DMap[("row",DInteger 8)]),("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])])
        @?=
        "Incorrect coordinate info."

    , testCase "Missing corresponding row info" $
        show (fromRight (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]) 
        (DMap[("Hints",DMap[("coords",DMap[("head",DMap[("col",DInteger 6)]),("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])])
        @?=
        "Incorrect coordinate info."

    , testCase "Missing corresponding head info" $
        show (fromRight (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]) 
        (DMap[("Hints",DMap[("coords",DMap[("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])])
        @?=
        "Incorrect coordinate info."

    , testCase "Missing corresponding tail info" $
        show (fromRight (State [("toggles",DString ""),
        ("hints",DString ""),
        ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
        ("occupied_rows",DString "1123142420"),
        ("occupied_cols",DString "2022206033"),
        ("number_of_hints",DInteger 10)]) 
        (DMap[("Hints",DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)])])])])])
        @?=
        "Incorrect coordinate info."

  ]