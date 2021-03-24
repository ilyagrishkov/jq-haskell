module Jq.Filters where

import Jq.Json (JSON)

data Filter = Identity | ObjectIdentifierIndex String | OptionalObjectIdentifierIndex String
  | ArrayIndex [Int] | OptionalArrayIndex [Int] | Slice (Int, Int) | OptionalSlice (Int, Int) 
  | Comma (Filter, Filter) | Pipe (Filter, Filter)
  | JSONVal JSON | ArrayConstructor Filter | ObjectConstructor [(String, Filter)] | ArrayExpander Filter
  deriving (Show)

data Config = ConfigC {filters :: Filter}
