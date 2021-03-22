module Jq.Filters where

data Filter = Identity | ObjectIdentifierIndex String | OptionalObjectIdentifierIndex String
  | ArrayIndex [Int] | OptionalArrayIndex [Int] | Slice (Int, Int) | Comma (Filter, Filter) | Pipe (Filter, Filter)
  deriving (Show)

data Config = ConfigC {filters :: Filter}
