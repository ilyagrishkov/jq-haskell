module Jq.Json where

indent :: Int
indent = 2
  
data JSON =
    JNull | JNum Double | JBool Bool | JString String | JArray [JSON] | JObject [(String, JSON)]
    deriving (Eq)
    
instance Show JSON where
  show = prettyPrint 0

formatObject ::  Int -> [(String, JSON)] -> String
formatObject _ [] = ""
formatObject n [(k, v)] = replicate n ' ' ++ show k ++ ": " ++ prettyPrint n v
formatObject n ((k, v):xs) = replicate n ' ' ++ show k ++ ": " ++ prettyPrint n v ++ ",\n" ++ formatObject n xs

formatArray ::  Int -> [JSON] -> String
formatArray _ [] = ""
formatArray n [x] = replicate n ' ' ++ prettyPrint n x
formatArray n (x:xs) = replicate n ' ' ++ prettyPrint n x ++ ",\n" ++ formatArray n xs

prettyPrint :: Int -> JSON -> String
prettyPrint _ JNull = "null"
prettyPrint _ (JNum v) = if v == fromInteger (round v) then show (round v :: Integer) else show v
prettyPrint _ (JBool b) = show b
prettyPrint _ (JString s) = show s
prettyPrint n (JArray x) = "[\n" ++ formatArray (n + indent) x ++ "\n" ++ replicate n ' ' ++ "]"
prettyPrint n (JObject x) = "{\n" ++ formatObject (n + indent) x ++ "\n" ++ replicate n ' ' ++ "}"
