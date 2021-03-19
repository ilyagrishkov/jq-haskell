module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity = \inp -> Right [inp]
compile ValueIterator = compileValueIterator
compile (ObjectIdentifierIndex i) = compileObjectIdentifierIndex i
compile (OptionalObjectIdentifierIndex i) = compileOptionalObjectIdentifierIndex i
compile (GenericObjectIndex i) = compileObjectIdentifierIndex i
compile (ArrayIndex i) = compileArrayIndex i
compile (Slice s) = uncurry compileSlice s
compile (Comma f) = uncurry compileComma f
compile (Pipe f) = uncurry compilePipe f

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

compileArrayIndex :: Int -> JProgram[JSON]
compileArrayIndex i (JArray a) = Right [a !! i]
compileArrayIndex _ _ = Left "cannot select element from non-array"

compileSlice :: Int -> Int -> JProgram[JSON]
compileSlice s t (JArray a) = Right (take t (drop s a))
compileSlice _ _ _ = Left "cannot slice over non array element"

compileComma :: Filter -> Filter -> JProgram [JSON]
compileComma f1 f2 inp = (++) <$> compile f1 inp <*> compile f2 inp

compilePipe :: Filter -> Filter -> JProgram [JSON]
compilePipe f1 f2 inp = compile f1 inp >>= compileSecondPipe f2
      
compileSecondPipe :: Filter -> [JSON] -> Either String [JSON]
compileSecondPipe _ [] = Right [JNull]
compileSecondPipe f [x] = compile f x
compileSecondPipe f (x:xs) = (++) <$> compile f x <*> compileSecondPipe f xs
      
run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
