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

parseComma :: Parser Filter
parseComma = Comma <$> filtersPair
   where
     filtersPair = (\key _ val -> (key, val)) <$> parseFilter <*> (space *> char ',' <* space) <*> parseFilter

parseFilter :: Parser Filter
parseFilter = token (parseArrayIndex <|> parseGenericObjectIndex <|> parseOptionalObjectIdentifierIndex <|> parseObjectIdentifierIndex <|> parseValueIterator <|> parseIdentity <|> parseComma)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
