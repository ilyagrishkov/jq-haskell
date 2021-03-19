module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = Identity <$ char '.'
  
parseValueIterator :: Parser Filter
parseValueIterator = ValueIterator <$ string ".[]"

parseObjectIdentifierIndex :: Parser Filter
parseObjectIdentifierIndex = ObjectIdentifierIndex <$> (char '.' *> identifier)

parseOptionalObjectIdentifierIndex :: Parser Filter
parseOptionalObjectIdentifierIndex = OptionalObjectIdentifierIndex <$> (char '.' *> identifier <* char '?')

parseGenericObjectIndex :: Parser Filter
parseGenericObjectIndex = GenericObjectIndex <$> (string ".[" *> charSeq <* string "]")

parseArrayIndex :: Parser Filter
parseArrayIndex = ArrayIndex <$> (string ".[" *> integer <* char ']')

parseSlice :: Parser Filter
parseSlice = Slice <$> (string ".[" *> slice <* char ']')
  where 
    slice = (\a _ c -> (a, c)) <$> integer <*> string ":" <*> integer
  
parseComma :: Parser Filter
parseComma = Comma <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> parseSingleFilter <*> (space *> char ',' <* space) <*> parseFilter
     
parsePipe :: Parser Filter
parsePipe = Pipe <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> parseSingleFilter <*> (space *> char '|' <* space) <*> parseFilter
     
parseNestedObjectIndex :: Parser Filter
parseNestedObjectIndex = Pipe <$> filtersPair
  where
     filtersPair = (\key val -> (ObjectIdentifierIndex key, val)) <$> (char '.' *> identifier) <*> parseNestedObjectIndex

parseFilter :: Parser Filter
parseFilter = parseComplexFilter <|> parseSingleFilter

parseSingleFilter :: Parser Filter
parseSingleFilter = token (parseSlice <|> parseArrayIndex <|> parseGenericObjectIndex <|> parseOptionalObjectIdentifierIndex <|> parseObjectIdentifierIndex <|> parseValueIterator <|> parseIdentity)

parseComplexFilter :: Parser Filter 
parseComplexFilter = token (parseComma <|> parsePipe <|> parseNestedObjectIndex)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
