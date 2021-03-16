module Jq.Filters where

data Filter = Identity | ObjectIdentifierIndex String | OptionalObjectIdentifierIndex String 
  | GenericObjectIndex String | ArrayIndex Int | Slice Int Int | ValueIterator 
  | Comma Filter Filter | Pipe Filter Filter 

instance Show Filter where
  show (Identity) = "."
  show ValueIterator = ".[]"

data Config = ConfigC {filters :: Filter}
