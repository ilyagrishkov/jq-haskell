module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = Identity <$ char '.'
  
parseValueIterator :: Parser Filter
parseValueIterator = ValueIterator <$ string ".[]"

parseFilter :: Parser Filter
parseFilter = token (parseValueIterator <|> parseIdentity)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
