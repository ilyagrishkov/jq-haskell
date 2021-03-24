module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser (parseJSON)


-----------------------------

parseFilter :: Parser Filter
parseFilter =  parseJsonFilter <|> parseArrayConstructor <|> parseObjectConstructor <|> parseComplexFilter <|> parseSingleFilter

parseSingleFilter :: Parser Filter
parseSingleFilter = token (parseGroup <|> parseSequentialFilters)

parseComplexFilter :: Parser Filter
parseComplexFilter = token (parsePipe <|> parseComma)

-----------------------------
--- Filter parsers

parseIdentity :: Parser Filter
parseIdentity = Identity <$ char '.'

parseSequentialFilters :: Parser Filter
parseSequentialFilters = convertToPipe <$> ((:) <$> parsePrimaryFilters <*> many parseSecondaryFilters)

parsePrimaryFilters :: Parser Filter
parsePrimaryFilters = parseOptionalSlice <|> parseSlice <|> parseOptionalArrayIndex <|> parseArrayIndex 
  <|> parseOptionalObjectIdentifierIndex <|> parseStandardObjectIdentifierIndex <|> parseIdentity
  
parseSecondaryFilters :: Parser Filter
parseSecondaryFilters = parseOptionalSlice <|> parseSlice <|> parseOptionalArrayIndex <|> parseArrayIndex 
  <|> parseOptionalObjectIdentifierIndex <|> parseStandardObjectIdentifierIndex
  
convertToPipe :: [Filter] -> Filter
convertToPipe [] = Identity
convertToPipe (x:xs) = Pipe (x, convertToPipe xs)

parseOptionalObjectIdentifierIndex :: Parser Filter
parseOptionalObjectIdentifierIndex = OptionalObjectIdentifierIndex <$> (char '.' *> (identifier <* string "?" <|> charSeq <* string "?" <|> (string "[" *> charSeq <* string "]?")))

parseStandardObjectIdentifierIndex :: Parser Filter 
parseStandardObjectIdentifierIndex = ObjectIdentifierIndex <$> (char '.' *> (identifier <|> charSeq <|> (string "[" *> charSeq <* string "]")))

parseArrayIndex :: Parser Filter
parseArrayIndex = ArrayIndex <$> (string "[" *> split (space *> char ',' <* space) integer <* char ']')

parseOptionalArrayIndex :: Parser Filter
parseOptionalArrayIndex = OptionalArrayIndex <$> (string "[" *> split (space *> char ',' <* space) integer <* string "]?")

parseSlice :: Parser Filter
parseSlice = Slice <$> (string "[" *> slice <* char ']')
  where 
    slice = (\a _ c -> (a, c)) <$> integer <*> string ":" <*> integer

parseOptionalSlice :: Parser Filter
parseOptionalSlice = OptionalSlice <$> (string "[" *> slice <* string "]?")
  where
    slice = (\a _ c -> (a, c)) <$> integer <*> string ":" <*> integer
  
parseComma :: Parser Filter
parseComma = Comma <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> parseSingleFilter <*> (space *> char ',' <* space) <*> (parseComma <|> parseSingleFilter)
     
parsePipe :: Parser Filter
parsePipe = Pipe <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> (parseComma <|> parseSingleFilter) <*> (space *> char '|' <* space) <*> parseFilter

parseGroup :: Parser Filter
parseGroup = char '(' *> parseFilter <* char ')'

-----------------------------
--- Value constructors parsers

parseJsonFilter :: Parser Filter
parseJsonFilter = JSONVal <$> parseJSON

parseArrayConstructor :: Parser Filter
parseArrayConstructor = ArrayConstructor <$> (char '[' *> parseFilter <* char ']')

parseObjectConstructor :: Parser Filter
parseObjectConstructor = ObjectConstructor <$> (char '{' *> space *> split (space *> char ',' <* space) keyValue <* space <* char '}')
  where
    keyValue = (\key _ val -> (key, val)) <$> (charSeq <|> identifier) <*> (space *> char ':' <* space) <*> parseFilter

-----------------------------

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
