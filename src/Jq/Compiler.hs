module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile ValueIterator inp = compileValueIterator inp

compileValueIterator :: JProgram [JSON]
compileValueIterator (JObject b) = return (map snd b)
compileValueIterator (JArray a) = return a
compileValueIterator JNull = error "Cannot iterate over null (null)" 
compileValueIterator (JNum n) = error ("Cannot iterate over number (" ++ show (JNum n) ++ ")")
compileValueIterator (JString s) = error ("Cannot iterate over string (" ++ s ++ ")")
compileValueIterator (JBool b) = error ("Cannot iterate over boolean (" ++ show b ++ ")")

  

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
