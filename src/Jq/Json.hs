module Jq.Json where

data JSON =
    JNull | JNum Double | JBool Bool | JString String | JArray [JSON] | JObject[(String, JSON)]
    deriving (Eq)
    
instance Show JSON where
  show JNull = "null"
  show (JNum n) = if n == fromInteger (round n) then show (round n :: Integer) else show n
  show (JBool b) = show b
  show (JString s) = show s
  show (JArray a) = show a
  show (JObject o) = "{" ++ formatObject o ++ "}"

formatObject :: Show a => [(String, a)] -> String
formatObject [] = ""
formatObject [(k, v)] = show k ++ ":" ++ show v
formatObject ((k, v):xs) = show k ++ ":" ++ show v ++ "," ++ formatObject xs