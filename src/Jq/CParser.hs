module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = Identity <$ char '.'
  
parseValueIterator :: Parser Filter
parseValueIterator = ValueIterator . (\x  -> last x == '?') <$> (string ".[]?" <|> string ".[]")

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

parseGroup :: Parser Filter
parseGroup = char '(' *> parseFilter <* char ')'

parseFilter :: Parser Filter
parseFilter = parseComplexFilter <|> parseSingleFilter

parseSingleFilter :: Parser Filter
parseSingleFilter = token (parseGroup <|> parseSlice <|> parseArrayIndex <|> parseObjectIdentifierIndex <|> parseValueIterator <|> parseIdentity)

parseComplexFilter :: Parser Filter
parseComplexFilter = token (parsePipe <|> parseComma)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
