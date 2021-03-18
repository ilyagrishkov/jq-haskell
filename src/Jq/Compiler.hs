module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity = \inp -> Right [inp]
compile ValueIterator = compileValueIterator
compile (ObjectIdentifierIndex i) = compileObjectIdentifierIndex i
compile (OptionalObjectIdentifierIndex i) = compileOptionalObjectIdentifierIndex i
compile (GenericObjectIndex i) = undefined
compile (ArrayIndex i) = undefined
compile (Slice s) = undefined
compile (Comma f) = uncurry compileComma f 
compile (Pipe f) = undefined

compileValueIterator :: JProgram [JSON]
compileValueIterator (JObject b) = return (map snd b)
compileValueIterator (JArray a) = return a
compileValueIterator JNull = error "Cannot iterate over null (null)" 
compileValueIterator (JNum n) = error ("Cannot iterate over number (" ++ show (JNum n) ++ ")")
compileValueIterator (JString s) = error ("Cannot iterate over string (" ++ s ++ ")")
compileValueIterator (JBool b) = error ("Cannot iterate over boolean (" ++ show b ++ ")")

compileObjectIdentifierIndex :: String -> JProgram [JSON]
compileObjectIdentifierIndex i (JObject o) = 
  case map snd (filter (\(a, _) -> a == i) o) of
    [] -> Left "couldn't select anything"
    arr -> Right arr
compileObjectIdentifierIndex _ _ = Left "couldn't select anything"

compileOptionalObjectIdentifierIndex :: String -> JProgram [JSON]
compileOptionalObjectIdentifierIndex i (JObject o) = 
  case map snd (filter (\(a, _) -> a == i) o) of
    [] -> Right [JNull]
    arr -> Right arr
compileOptionalObjectIdentifierIndex _ _ = Right [JNull]

compileComma :: Filter -> Filter -> JProgram [JSON]
compileComma f1 f2 inp = (++) <$> compile f1 inp <*> compile f2 inp

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
