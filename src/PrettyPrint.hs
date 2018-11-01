module PrettyPrint where
import Parser
fromJson :: JValue -> String
fromJson jvalue  = prettyPrint 0 jvalue

prettyPrint :: Int -> JValue -> String
prettyPrint i (JString s) = s
prettyPrint i (JNumber num) = show num
prettyPrint i (JArray arr) =
  let indent = replicate i '\t'
      lineIndent = replicate (i+1) '\t'
  in
    "[\n" ++ lineIndent ++ ( concat $ map (++ "\n" ++ lineIndent) $ map (prettyPrint (i + 1)) arr) ++ indent ++ "]"
prettyPrint i (JObject os) =
  "{\n" ++ printObjects (i+1) os ++ "}"

printObjects :: Int -> [(String, JValue)] -> String
printObjects _ [] = ""
printObjects i ((k, v):os) =
  let indent = replicate i '\t' in
  indent ++ k ++ ": " ++ prettyPrint i v ++ "\n" ++ printObjects i os
