import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

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
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []