module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity = \inp -> Right [inp]
compile (ObjectIdentifierIndex i) = compileObjectIdentifierIndex i False
compile (OptionalObjectIdentifierIndex i) = compileObjectIdentifierIndex i True
compile (ArrayIndex i) = compileArrayIndex i False
compile (OptionalArrayIndex i) = compileArrayIndex i True
compile (Slice s) = uncurry compileSlice s False
compile (OptionalSlice s) = uncurry compileSlice s True
compile (Comma f) = uncurry compileComma f
compile (Pipe f) = uncurry compilePipe f
compile (JSONVal v) = \_ -> Right [v]
compile (ArrayConstructor a) = compileArrayConstructor a
compile (ObjectConstructor o) = compileObjectConstructor o
compile EmptyFilter = error "Empty filter should not appear"

----------------------------- 
--- Filter compilers

compileValueIterator :: Bool -> JProgram [JSON]
compileValueIterator _ (JObject b) = Right (map snd b)
compileValueIterator _ (JArray a) = Right a
compileValueIterator opt _ = if opt then Right [] else Left "Cannot iterate"

compileObjectIdentifierIndex :: String -> Bool -> JProgram [JSON]
compileObjectIdentifierIndex i opt (JObject o) =
  case map snd (filter (\(a, _) -> a == i) o) of
    [] -> if opt then Right [JNull] else Left "couldn't select anything"
    arr -> Right [last arr]
compileObjectIdentifierIndex _ _ JNull = Right [JNull]
compileObjectIdentifierIndex _ opt _ = if opt then Right [] else Left "couldn't select anything"

compileArrayIndex :: [Int] -> Bool -> JProgram[JSON]
compileArrayIndex [] _ (JObject b) = Right (map snd b)
compileArrayIndex [] _ (JArray a) = Right a
compileArrayIndex i _ (JArray a) = Right (map (`findElem` a) i)
compileArrayIndex _ opt _ = if opt then Right [] else Left "cannot select element from non-array"

compileSlice :: Int -> Int -> Bool -> JProgram[JSON]
compileSlice s t _ (JArray a) = Right [JArray (take (t - 1) (drop s a))]
compileSlice s t _ (JString str) = Right [JString (take (t - 1) (drop s str))]
compileSlice _ _ _ JNull = Right [JNull]
compileSlice _ _ opt _ = if opt then Right [] else Left "cannot slice over non array element"

compileComma :: Filter -> Filter -> JProgram [JSON]
compileComma f1 f2 inp = (++) <$> (compile f1 inp <> Right [JNull]) <*> (compile f2 inp <> Right [JNull])

compilePipe :: Filter -> Filter -> JProgram [JSON]
compilePipe f1 f2 inp = compile f1 inp >>= foldr (\x -> (<*>) ((++) <$> compile f2 x)) (Right [])
      
findElem :: Int -> [JSON] -> JSON
findElem _ [] = JNull
findElem 0 (x:_) = x
findElem i (_:xs) = if i - 1 >= length xs then JNull else findElem (i - 1) xs


----------------------------- 
--- Value Constructors

compileArrayConstructor :: Filter -> JProgram [JSON]
compileArrayConstructor f inp = case compile f inp of
  Left _ -> Right [JArray []]
  Right x -> Right [JArray x]

compileObjectConstructor :: [(Filter, Filter)] -> JProgram [JSON]
compileObjectConstructor [] _ = error "Expected non-empty object constrcutor" -- should never occur
compileObjectConstructor [jElem] inp = compileSingleObjectConstructor jElem inp
compileObjectConstructor (jElem:jElems) inp = case (compileSingleObjectConstructor jElem inp, compileObjectConstructor jElems inp) of
  (Right x1, Right x2) -> Right [JObject (y ++ o) | (JObject y) <- x1, (JObject o) <- x2]
  (Right _, Left x) -> Left x
  (Left x, _) -> Left x

compileSingleObjectConstructor :: (Filter, Filter) -> JProgram [JSON]
compileSingleObjectConstructor (JSONVal (JString a), EmptyFilter) inp = case compile (ObjectIdentifierIndex a) inp of
  Right x -> Right [JObject [(a, y)] | y <- x]
  Left _ -> Right [JObject [(a, JNull)]]
compileSingleObjectConstructor (JSONVal (JString a), f) inp = case compile f inp of
  Right x -> Right [JObject [(a, y)] | y <- x]
  Left _ -> Right [JObject [(a, JNull)]]
compileSingleObjectConstructor (a, f) inp = case (compile a inp, compile f inp) of
  (Right [JString s], Right x) -> Right [JObject [(s, y)] | y <- x]
  (Right _, Right _) -> Left "unable to use non string key"
  (Right _, Left x) -> Left x
  (Left x, _) -> Left x
  
----------------------------- 
    
run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
