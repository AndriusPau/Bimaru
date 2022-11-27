import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode, encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat )

import Lib1 (State(..))
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" 
    [
    toYamlTests,
    fromYamlTests,
    -- gameStartTests,
    -- hintTests,
    properties
    ])


properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (cs ((Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc))) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "Null value" $
        parseDocument "null" @?= Right DNull
    ,  testCase "String without quotes" $
        parseDocument "ThisIsAString" @?= Right (DString "ThisIsAString")
    ,  testCase "String with quotes" $
        parseDocument "\"This is a string\"" @?= Right (DString "This is a string")
    ,  testCase "Positive integer" $
        parseDocument "123" @?= Right (DInteger 123)
    ,  testCase "Negative integer" $
        parseDocument "-123" @?= Right (DInteger (-123))
    ,  testCase "List of strings without quotes" $
        parseDocument "- ThisIsAString\n- ThisIsAlsoAString\n- ThisIsNotANumber" @?= Right (DList [DString "ThisIsAString", DString "ThisIsAlsoAString", DString "ThisIsNotANumber"])
    ,  testCase "List of strings with quotes" $
        parseDocument "- \"This is a string\"\n- \"This is also a string\"\n- \"This is a list of chars\"" @?= Right (DList [DString "This is a string", DString "This is also a string", DString "This is a list of chars"])
    ,  testCase "List of integers" $
        parseDocument "- 123\n- -456\n- 789" @?= Right (DList [DInteger 123, DInteger (-456), DInteger 789])
    ,  testCase "List of DMaps" $
        parseDocument "- DMap1: 123\n  DMap2: 456\n- DMap1: 789\n  DMap2: 101112" @?= Right (DList [DMap [("DMap1", DInteger 123), ("DMap2", DInteger 456)], DMap [("DMap1", DInteger 789), ("DMap2", DInteger 101112)]])
    ,  testCase "List of DMaps with DLists inside" $
        parseDocument "- DMap1: 123\n  DMap2: 456\n  DMap3:\n  - 789\n  - 101112\n- DMap1: 131415\n  DMap2: 161718\n  DMap3:\n  - 192021\n  - 222324" @?= Right (DList [DMap [("DMap1", DInteger 123), ("DMap2", DInteger 456), ("DMap3", DList [DInteger 789, DInteger 101112])], DMap [("DMap1", DInteger 131415), ("DMap2", DInteger 161718), ("DMap3", DList [DInteger 192021, DInteger 222324])]])
    ,  testCase "DMap with DLists inside" $
        parseDocument "DMap1: 123\nDMap2: 456\nDMap3:\n- 789\n- 101112" @?= Right (DMap [("DMap1", DInteger 123), ("DMap2", DInteger 456), ("DMap3", DList [DInteger 789, DInteger 101112])])
  ]


toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "Null" $
        renderDocument DNull @?= "---\nnull"

    , testCase "Int" $
        renderDocument (DInteger 789123) @?= "---\n789123"
    , testCase "String" $
        renderDocument (DString "Test123") @?= "---\n\"Test123\""


    , testCase "Empty List" $
        renderDocument (DList []) @?= "---\n[]"
    , testCase "List of Dints" $
        renderDocument (DList [DInteger 1, DInteger 2, DInteger 3, DInteger 4, DInteger 5, DInteger 6, DInteger 7, DInteger 111]) @?= listOfDInts
    , testCase "List of Dnulls" $
        renderDocument (DList [DNull, DNull, DNull]) @?= listOfDNulls
    , testCase "List of Dstrings" $
        renderDocument (DList [DString "Hello", DString "World", DString "!!"]) @?= listOfDStrings
    , testCase "List of DMaps" $
        renderDocument (DList[DMap[("1.1", DInteger 1), ("1.2", DInteger 2)], DMap[("2.1", DInteger 1)], DMap[("3.1", DInteger 1), ("3.2", DInteger 2)]]) @?= listOfDMaps
    , testCase "List of DMaps2" $
        renderDocument (DList[DMap[("1.0", DInteger 1), ("1.2DMap", DMap[("1.2.4", DMap[("2.1.6", DMap[])])])], DMap[("2.0", DInteger 1)], DMap[("3.0", DInteger 1), ("3.2", DInteger 2)]]) @?= listOfDMaps2

    , testCase "Empty DMap" $
        renderDocument (DMap[]) @?= "---\n{}"
    , testCase "DMap with one element" $
        renderDocument (DMap[("DNull", DNull)]) @?= "---\nDNull: null"
    , testCase "DMap with list of ints inside" $
        renderDocument (DMap [("List", DList[DInteger 1, DInteger 2, DInteger 3, DInteger 4])]) @?= "---\nList:\n- 1\n- 2\n- 3\n- 4"
    , testCase "DMap with a DMap which has a DMap inside" $
        renderDocument (DMap[("first", DMap[("second", DMap[("third", DString "The end")])])]) @?= "---\nfirst:\n  second:\n    third: \"The end\""
    , testCase "DMap with a DMap which has a DMap with a List inside" $
        renderDocument (DMap[("first", DMap[("second", DMap[("List", DList[DInteger 1, DInteger 2, DList[]])])])]) @?= "---\nfirst:\n  second:\n    List:\n    - 1\n    - 2\n    - []"
    , testCase "DMap with a DMap which has a DMap with a List inside" $
        renderDocument (DMap[("first", DMap[("second", DMap[("third", DList[DInteger 1, DInteger 2])])])]) @?= "---\nfirst:\n  second:\n    third:\n    - 1\n    - 2"

    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

listOfDInts :: String
listOfDInts = "---\n- 1\n- 2\n- 3\n- 4\n- 5\n- 6\n- 7\n- 111"

listOfDNulls :: String
listOfDNulls = "---\n- null\n- null\n- null"

listOfDStrings :: String
listOfDStrings = "---\n- \"Hello\"\n- \"World\"\n- \"!!\""

listOfDMaps :: String
listOfDMaps = "---\n- 1.1: 1\n  1.2: 2\n- 2.1: 1\n- 3.1: 1\n  3.2: 2"

listOfDMaps2 :: String
listOfDMaps2 = "---\n- 1.0: 1\n  1.2DMap:\n    1.2.4:\n      2.1.6: {}\n- 2.0: 1\n- 3.0: 1\n  3.2: 2"
    
-- gameStartTests :: TestTree
-- gameStartTests = testGroup "Test start document"
--   [   testCase "No information missing" $
--         show (fromRight (State[]) (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
--         ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
--         ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")])))
--         @?=
--         show (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])

--     ,   testCase "Extra information added" $
--         show (fromRight (State[]) (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
--         ("additional_info1", DString "Additional info"),
--         ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
--         ("additional_info2", DInteger 420),
--         ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
--         ("additional_info3", DMap [("additional_info_inside_additional_info", DList [DNull, DInteger 6, DInteger 9])]),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")])))
--         @?=
--         show (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])

--     , testCase "Missing Collumn info" $
--         fromLeft "(State[])" (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
--         ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")]))
--         @?=
--         "Occupied collumns information is missing."

--     , testCase "Missing Row info" $
--         fromLeft "(State[])" (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
--         ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")]))
--         @?=
--         "Occupied rows information is missing."

--     , testCase "Missing Hint info" $
--         fromLeft "(State[])" (gameStart (State []) (DMap [
--         ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
--         ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0]),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b")]))
--         @?=
--         "Hint information is missing."

--     , testCase "Missing Game Setup Id info" $
--         fromLeft "(State[])" (gameStart (State []) (DMap [("number_of_hints",DInteger 10),
--         ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
--         ("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 2,DInteger 3,DInteger 1,DInteger 4,DInteger 2,DInteger 4,DInteger 2,DInteger 0])]))
--         @?=
--         "Game setup Id not found."

--   ]

-- hintTests :: TestTree
-- hintTests = testGroup "Test hint document" [hintTestsServer, hintTestsState]

-- hintTestsServer :: TestTree
-- hintTestsServer = testGroup "Test hint document server side."
--   [
--       testCase "No information missing" $
--         show (fromRight (State []) (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),
--         ("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])))
--         @?=
--         show (State [("toggles",DString ""),
--         ("hints",DString "6867"),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])

--     , testCase "Missing corresponding row info" $
--         fromLeft "(State [])" (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("head",DMap[("col",DInteger 6)]),
--         ("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])]))
--         @?=
--         "Incorrect coordinate info."

--     , testCase "Missing corresponding collumn info" $
--         fromLeft "(State [])" (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("head",DMap[("row",DInteger 8)]),
--         ("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])]))
--         @?=
--         "Incorrect coordinate info."

--     , testCase "Missing corresponding head info" $
--         fromLeft "(State [])" (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])]))
--         @?=
--         "Incorrect coordinate info."

--     , testCase "Missing corresponding tail info" $
--         fromLeft "(State [])" (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),
--         ("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)])])])]))
--         @?=
--         "Incorrect coordinate info."

--     , testCase "\"Hint 0\" given to server" $
--         show (fromRight (State []) (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DNull)])))
--         @?=
--         show (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--   ]


-- hintTestsState :: TestTree
-- hintTestsState = testGroup "Test State info"
--     [
--       testCase "No information missing" $
--         show (fromRight (State []) (hint (State [("toggles",DString ""),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap [("coords",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 8)]),
--         ("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])))
--         @?=
--         show (State [("toggles",DString ""),
--         ("hints",DString "6867"),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])

--     , testCase "Extra information added" $
--         show (fromRight (State []) (hint (State [("toggles",DString ""),
--         ("Extra information",DString "Extra information"),
--         ("hints",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("Extra information",DNull),
--         ("occupied_rows",DString "1123142420"),
--         ("Extra information",DInteger 420),
--         ("occupied_cols",DString "2022206033"),
--         ("Extra information",DMap [("Extra information inside extra information", DMap [("Do you know Joe?", DString "Who's Joe?")])]),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),
--         ("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])])))
--         @?=
--         show (State [("toggles",DString ""),
--         ("Extra information",DString "Extra information"),
--         ("hints",DString "6867"),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("Extra information",DNull),
--         ("occupied_rows",DString "1123142420"),
--         ("Extra information",DInteger 420),
--         ("occupied_cols",DString "2022206033"),
--         ("Extra information",DMap[("Extra information inside extra information",DMap[("Do you know Joe?",DString "Who's Joe?")])]),
--         ("number_of_hints",DInteger 10)])

--     , testCase "Missing Hint info" $
--         fromLeft "(State [])" (hint (State [("toggles",DString ""),
--         ("game_setup_id",DString "3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
--         ("occupied_rows",DString "1123142420"),
--         ("occupied_cols",DString "2022206033"),
--         ("number_of_hints",DInteger 10)])
--         (DMap[("coords",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 8)]),
--         ("tail",DMap[("head",DMap[("col",DInteger 6),("row",DInteger 7)]),("tail",DNull)])])]))
--         @?=
--         "Hints not found in passed state."
--     ]
