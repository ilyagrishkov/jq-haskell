module Jq.JParser where

import Jq.Json
import Parsing.Parsing

parseNull :: Parser JSON
parseNull = JNull <$ string "null"

parseBool :: Parser JSON
parseBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JBool True <$ string "true"
    jsonFalse = JBool False <$ string "false"

parseNum :: Parser JSON
parseNum = JNum <$> parseDouble

parseDouble :: Parser Double
parseDouble = (\num e -> num * (10 ^^ e)) . read <$> ((++) <$> real <*> (('.' :) . show <$> float)) <*> expo
    where
      real = show <$> (integer <|> pure 0) 
      float = string "." *> (integer <|> pure 0) <|> pure 0
      expo = (char 'e' <|> char 'E') *> (integer <|> char '+' *> integer) <|> pure 0

parseString :: Parser JSON
parseString = JString <$> charSeq

parseArray :: Parser JSON
parseArray = JArray <$> (char '[' *> space *> elems <* space <* char ']')
  where
    elems = sepBy (space *> char ',' <* space) parseJSON

parseObject :: Parser JSON
parseObject = JObject <$> (char '{' *> space *> sepBy (space *> char ',' <* space) keyValue <* space <* char '}')
  where
    keyValue = (\key _ val -> (key, val)) <$> charSeq <*> (space *> char ':' <* space) <*> parseJSON

parseJSON :: Parser JSON
parseJSON = token (parseNull <|> parseBool <|> parseNum <|> parseString <|> parseArray <|> parseObject)
