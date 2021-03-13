module Jq.JParser where

import Parsing.Parsing
import Jq.Json

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
parseDouble = createDouble 
    <$> integer
    <*> ((read <$> (('0':) <$> ((:) <$> char '.' <*> (show <$> integer)))) <|> pure 0)
    <*> ((char 'e' <|> char 'E') *> (integer <|> char '+' *> integer <|> pure 0))

createDouble :: Int -> Double -> Int -> Double
createDouble real float expo = (fromIntegral real + float) * (10 ^^ expo)
   
charSeq :: Parser String
charSeq = char '\"' *> token str <* char '\"'
  where
    str = do x  <- sat (/= '\"')
             xs <- many (sat (/= '\"'))
             return (x:xs)

parseString :: Parser JSON
parseString = JString <$> charSeq

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

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
