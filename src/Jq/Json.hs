module Jq.Json where

indent :: Int
indent = 2
  
data JSON =
    JNull | JNum Double | JBool Bool | JString String | JArray [JSON] | JObject [(String, JSON)]
    deriving (Eq)
    
instance Show JSON where
  show = prettyPrintColor 0

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
prettyPrint _ (JBool b) = if b then "true" else "false"
prettyPrint _ (JString s) = show s
prettyPrint n (JArray x) = if not (null x) then "[\n" ++ formatArray (n + indent) x ++ "\n" ++ replicate n ' ' ++ "]" else "[]"
prettyPrint n (JObject x) = if not (null x) then "{\n" ++ formatObject (n + indent) x ++ "\n" ++ replicate n ' ' ++ "}" else "{}"

-----------------------------
--- Color printing

blue :: String
blue = "\x1b[34m"

black :: String
black = "\x1b[30m"

green :: String
green = "\x1b[32m"

resetColor :: String
resetColor = "\x1b[0m"

formatObjectColor ::  Int -> [(String, JSON)] -> String
formatObjectColor _ [] = ""
formatObjectColor n [(k, v)] = replicate n ' ' ++ blue ++ show k ++ resetColor ++ ": " ++ prettyPrintColor n v
formatObjectColor n ((k, v):xs) = replicate n ' ' ++ blue ++ show k ++ resetColor ++ ": " ++ prettyPrintColor n v ++ ",\n" ++ formatObjectColor n xs

formatArrayColor ::  Int -> [JSON] -> String
formatArrayColor _ [] = ""
formatArrayColor n [x] = replicate n ' ' ++ prettyPrintColor n x
formatArrayColor n (x:xs) = replicate n ' ' ++ prettyPrintColor n x ++ ",\n" ++ formatArrayColor n xs

prettyPrintColor :: Int -> JSON -> String
prettyPrintColor _ JNull = black ++ "null" ++ resetColor
prettyPrintColor _ (JNum v) = if v == fromInteger (round v) then show (round v :: Integer) else show v
prettyPrintColor _ (JBool b) = if b then "true" else "false"
prettyPrintColor _ (JString s) = green ++ show s ++ resetColor
prettyPrintColor n (JArray x) = if not (null x) then "[\n" ++ formatArrayColor (n + indent) x ++ "\n" ++ replicate n ' ' ++ "]" else "[]"
prettyPrintColor n (JObject x) = if not (null x) then "{\n" ++ formatObjectColor (n + indent) x ++ "\n" ++ replicate n ' ' ++ "}" else "{}"
