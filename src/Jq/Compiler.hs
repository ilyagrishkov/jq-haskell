module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity = \inp -> Right [inp]
compile (ObjectIdentifierIndex i) = compileObjectIdentifierIndex i
compile (OptionalObjectIdentifierIndex i) = compileOptionalObjectIdentifierIndex i
compile (ArrayIndex i) = compileArrayIndex i
compile (Slice s) = uncurry compileSlice s
compile (Comma f) = uncurry compileComma f
compile (Pipe f) = uncurry compilePipe f

compileValueIterator :: Bool -> JProgram [JSON]
compileValueIterator _ (JObject b) = Right (map snd b)
compileValueIterator _ (JArray a) = Right a
compileValueIterator opt _ = if opt then Right [] else Left "Cannot iterate"

compileObjectIdentifierIndex :: String -> JProgram [JSON]
compileObjectIdentifierIndex i (JObject o) = 
  case map snd (filter (\(a, _) -> a == i) o) of
    [] -> Left "couldn't select anything"
    arr -> Right [last arr]
compileObjectIdentifierIndex _ _ = Left "couldn't select anything"

compileOptionalObjectIdentifierIndex :: String -> JProgram [JSON]
compileOptionalObjectIdentifierIndex i (JObject o) =
  case map snd (filter (\(a, _) -> a == i) o) of
    [] -> Right [JNull]
    arr -> Right arr
compileOptionalObjectIdentifierIndex _ _ = Right [JNull]

compileArrayIndex :: [Int] -> JProgram[JSON]
compileArrayIndex [] (JObject b) = Right (map snd b)
compileArrayIndex [] (JArray a) = Right a
compileArrayIndex i (JArray a) = Right (map (`findElem` a) i)
compileArrayIndex _ _ = Left "cannot select element from non-array"

compileSlice :: Int -> Int -> JProgram[JSON]
compileSlice s t (JArray a) = Right (take t (drop s a))
compileSlice _ _ _ = Left "cannot slice over non array element"

compileComma :: Filter -> Filter -> JProgram [JSON]
compileComma f1 f2 inp = (++) <$> compile f1 inp <*> compile f2 inp

compilePipe :: Filter -> Filter -> JProgram [JSON]
compilePipe f1 f2 inp = compile f1 inp >>= foldr (\x -> (<*>) ((++) <$> compile f2 x)) (Right [])
      
findElem :: Int -> [JSON] -> JSON
findElem _ [] = JNull
findElem 0 (x:_) = x
findElem i (_:xs) = if i - 1 >= length xs then JNull else findElem (i - 1) xs

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
