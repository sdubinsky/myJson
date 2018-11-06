module Main where
import Test.Tasty
import Test.Tasty.HUnit

import PrettyPrint
import Parser

main :: IO ()
main = do
  defaultMain (testGroup "JSON tests" [parseNumberTest
                                      ,parseStringTest
                                      ,parseSimpleArrayTest
                                      ,parseSimpleObjectTest
                                      ,parseNestedArrayTest
                                      ,parseNestedObjectTest
                                      ])

parseNumberTest :: TestTree
parseNumberTest =
  let input = "12345" in
    testCase "testing number parsing"
    (assertEqual "should parse a number"  (Right $ JNumber 12345.0) (toJson input))

parseStringTest :: TestTree
parseStringTest =
  let input = "\"asdfad\"" in
    testCase "testing string parsing"
    (assertEqual "should parse a string" (Right $ JString "asdfad") (toJson input))


parseSimpleArrayTest :: TestTree
parseSimpleArrayTest =
  let input = "[1,2,3,\"asdf\",4,5,6]" in
    testCase "testing simple array parsing"
    (assertEqual "should parse a simple array"
      (Right $ JArray [
          JNumber 1
          , JNumber 2
          , JNumber 3
          , JString "asdf"
          , JNumber 4
          , JNumber 5
          , JNumber 6])
     (toJson input))

parseSimpleObjectTest :: TestTree
parseSimpleObjectTest =
  let input = "{\"a\": 1, \"b\": 2}" in
    testCase "testing simple object parsing"
    (assertEqual "should parse a simple object"
     (Right $ JObject [("a", JNumber 1), ("b", JNumber 2)])
     (toJson input))


parseNestedArrayTest :: TestTree
parseNestedArrayTest =
  let input = "[1,2,3, [4,5,6], {\"a\": 1, \"b\": 2}, 7]" in
    testCase "testing nested array parsing"
    (assertEqual "should parse a nested array"
    (Right $ JArray [JNumber 1, JNumber 2, JNumber 3,
           JArray [JNumber 4, JNumber 5, JNumber 6],
           JObject [("a", JNumber 1), ("b", JNumber 2)], JNumber 7])
    (toJson input))

parseNestedObjectTest :: TestTree
parseNestedObjectTest =
  let input =
        "{\"a\": 1, \"b\": 2, \"c\": [1,2,3, [4,5,6], {\"a\": 1, \"b\": 2}], \"f\": \"awekfhjaeflk\"}" in
    testCase "testing nested object parsing"
    (assertEqual "should parse a nested object"
     (Right $ JObject [("a", JNumber 1), ("b", JNumber 2),
             ("c", JArray [JNumber 1, JNumber 2, JNumber 3,
           JArray [JNumber 4, JNumber 5, JNumber 6],
           JObject [("a", JNumber 1), ("b", JNumber 2)]]),
            ("f", JString "awekfhjaeflk")])
    (toJson input))
  
