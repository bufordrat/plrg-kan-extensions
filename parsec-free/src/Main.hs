module Main (main) where

import Data.Tree
import Data.Char
import Control.Monad.Reader
import Text.Parsec
import Text.Parsec.Free.Log (LogType)

type Parser a = ParsecT String () (ReaderT LogType IO) a

main :: IO ()
main = pure ()

data Expression 
  = Literal Int
  | Add Expression Expression
  | Multiply Expression Expression

-- intP :: Parser Int
-- intP = many1 (satisfy isDigit)


-- our old friend, the JSON ADT
data JSON
  = JNull
  | JBoolean Bool
  | JInteger Int
  | JFloat Double
  | JString String
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Eq, Show)

-- -- null parser
jNullP :: Parser JSON
jNullP = pure JNull <* string "null"

-- -- boolean parser
jBooleanP :: Parser JSON
jBooleanP =
  pure JBoolean <*> choice
  [ pure True <* string "true"
  , pure False <* string "false"
  ]

-- sequence of digit characters parser
digitsP :: Parser String
digitsP = many1 digit

-- signed sequence of digit characters parser
digitsWithSignP :: Num a => Parser (a, String)
digitsWithSignP = pure (,)
          <*> choice [ pure (-1) <* char '-', pure 1 ]
          <*> many1 digit

-- integer parser
jIntegerP :: Parser JSON
jIntegerP = let mkInt (sign, digits) = JInteger $ sign * read digits
        in pure mkInt <*> digitsWithSignP

-- -- float parser
jFloatP :: Parser JSON
jFloatP = let mkFloat (sign, a) b =
                JFloat $ sign * read (a <> "." <> b)
          in pure mkFloat
             <*> digitsWithSignP
             <* char '.'
             <*> many1 digit

-- -- helper parser for JSON string parser
stringP :: Parser String
stringP = between
      (char '"')
      (char '"')
      (many (satisfy (/= '"')))

-- -- string parser
jStringP :: Parser JSON
jStringP = pure JString <*> stringP

-- -- array parser
jArray :: Parser JSON
jArray = pure JArray
  <*> between
  (char '[' *> spaces)
  (spaces <* char ']')
  (sepBy jsonP (spaces
        *> char ','
        <* spaces))

-- helper parser for JSON object parser
keyValueP :: Parser (String, JSON)
keyValueP =
  pure (,)
  <*> stringP <* spaces <* char ':' <* spaces
  <*> jsonP

-- object parser
jObjectP :: Parser JSON
jObjectP = pure JObject
  <*> between
  (char '{' *> spaces)
  (spaces <* char '}')
  (sepBy keyValueP (spaces *> char ',' <* spaces))

-- json value parser
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

-- the parser that looks at the actual input string
inputP :: Parser JSON
inputP = jsonP <* eof
