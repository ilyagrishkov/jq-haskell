module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser (parseJSON)


-----------------------------

parseFilter :: Parser Filter
parseFilter = parseArrayExpander <|> parseComplexFilter <|> parseSingleFilter <|> parseJsonFilter
  <|> parseArrayConstructor <|> parseObjectConstructor

parseSingleFilter :: Parser Filter
parseSingleFilter = token (parseGroup <|> parseOptionalSlice <|> parseSlice <|> parseOptionalArrayIndex
  <|> parseArrayIndex <|> parseObjectIdentifierIndex <|> parseIdentity)

parseComplexFilter :: Parser Filter
parseComplexFilter = token (parsePipe <|> parseComma)

-----------------------------
--- Filter parsers


parseArrayExpander :: Parser Filter
parseArrayExpander = ArrayExpander <$> parseSingleFilter <* string "[]"

parseIdentity :: Parser Filter
parseIdentity = Identity <$ char '.'

parseObjectIdentifierIndex :: Parser Filter
parseObjectIdentifierIndex = convertToPipe <$> some (char '.' *> (parseOptionalObjectIdentifierIndex <|> parseStandardObjectIdentifierIndex))

convertToPipe :: [String] -> Filter
convertToPipe [] = Identity
convertToPipe [x] = if last x == '?' then OptionalObjectIdentifierIndex (init x) else ObjectIdentifierIndex x
convertToPipe (x:xs) = Pipe (convertToPipe [x], convertToPipe xs)

parseOptionalObjectIdentifierIndex :: Parser String
parseOptionalObjectIdentifierIndex = (++) <$> identifier <*> string "?" <|> (++) <$> charSeq <*> string "?" <|> (++) <$> (string "[" *> charSeq <* string "]") <*> string "?"

parseStandardObjectIdentifierIndex :: Parser String
parseStandardObjectIdentifierIndex = identifier <|> charSeq <|> (string "[" *> charSeq <* string "]")

parseArrayIndex :: Parser Filter
parseArrayIndex = ArrayIndex <$> (string ".[" *> split (space *> char ',' <* space) integer <* char ']')

parseOptionalArrayIndex :: Parser Filter
parseOptionalArrayIndex = OptionalArrayIndex <$> (string ".[" *> split (space *> char ',' <* space) integer <* string "]?")

parseSlice :: Parser Filter
parseSlice = Slice <$> (string ".[" *> slice <* char ']')
  where 
    slice = (\a _ c -> (a, c)) <$> integer <*> string ":" <*> integer

parseOptionalSlice :: Parser Filter
parseOptionalSlice = OptionalSlice <$> (string ".[" *> slice <* string "]?")
  where
    slice = (\a _ c -> (a, c)) <$> integer <*> string ":" <*> integer
  
parseComma :: Parser Filter
parseComma = Comma <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> (parseArrayExpander <|> parseSingleFilter) <*> (space *> char ',' <* space) <*> (parseArrayExpander <|> parseComma <|> parseSingleFilter)
     
parsePipe :: Parser Filter
parsePipe = Pipe <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> (parseArrayExpander <|> parseComma <|> parseSingleFilter) <*> (space *> char '|' <* space) <*> parseFilter

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
    keyValue = (\key _ val -> (key, val)) <$> charSeq <*> (space *> char ':' <* space) <*> parseFilter

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
