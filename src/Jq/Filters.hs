module Jq.Filters where

data Filter = Identity | ObjectIdentifierIndex String | OptionalObjectIdentifierIndex String
  | ArrayIndex [Int] | Slice (Int, Int) | ValueIterator
  | Comma (Filter, Filter) | Pipe (Filter, Filter)
  deriving (Show)

data Config = ConfigC {filters :: Filter}
