module Main (main) where

import Data.Tree
import Data.Char
import Control.Monad.Reader
import Text.Parsec hiding ((<|>), choice, sepBy, sepBy1, many, many1)
import qualified Text.Parsec as P
import Text.Parsec.Free.Log (LogType)
import Data.Functor.Identity

-- type Parser a = ParsecT String () (ReaderT LogType IO) a
type Parser a = ParsecT String () Identity a

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (P.try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . fmap P.try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try


data JSON
  = JNull
  | JBoolean Bool
  | JInteger Int
  | JFloat Double
  | JString String
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Eq, Show)

jNullP :: Parser JSON
jNullP = pure JNull <* string "null"

jBooleanP :: Parser JSON
jBooleanP =
  pure JBoolean <*> choice
  [ pure True <* string "true"
  , pure False <* string "false"
  ]

digitsP :: Parser String
digitsP = many1 digit

digitsWithSignP :: Num a => Parser (a, String)
digitsWithSignP = pure (,)
          <*> choice [ pure (-1) <* char '-', pure 1 ]
          <*> many1 digit

jIntegerP :: Parser JSON
jIntegerP = let mkInt (sign, digits) = JInteger $ sign * read digits
        in pure mkInt <*> digitsWithSignP

jFloatP :: Parser JSON
jFloatP = let mkFloat (sign, a) b =
                JFloat $ sign * read (a <> "." <> b)
          in pure mkFloat
             <*> digitsWithSignP
             <* char '.'
             <*> many1 digit

stringP :: Parser String
stringP = between
      (char '"')
      (char '"')
      (many (satisfy (/= '"')))

jStringP :: Parser JSON
jStringP = pure JString <*> stringP

jArray :: Parser JSON
jArray = pure JArray
  <*> between
  (char '[' *> spaces)
  (spaces <* char ']')
  (sepBy jsonP (spaces
        *> char ','
        <* spaces))

keyValueP :: Parser (String, JSON)
keyValueP =
  pure (,)
  <*> stringP <* spaces <* char ':' <* spaces
  <*> jsonP

jObjectP :: Parser JSON
jObjectP = pure JObject
  <*> between
  (char '{' *> spaces)
  (spaces <* char '}')
  (sepBy keyValueP (spaces *> char ',' <* spaces))

jsonP :: Parser JSON
jsonP = choice
    [ jNullP,
      jBooleanP,
      jFloatP,
      jIntegerP,
      jStringP,
      jArray,
      jObjectP
    ]

inputP :: Parser JSON
inputP = jsonP <* eof

main :: IO ()
main = pure ()
