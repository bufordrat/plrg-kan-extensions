{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , char
  , string
  , satisfy
  , eof
  , parserFail
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)


import Data.Tree
import Data.Char

-- Milewski right Kan extension datatype
newtype Ran k d a = Ran (forall i. (a -> k i) -> d i)

-- example where Tree functor is k
-- f :: Monoid a => [a] -> Tree a
-- f [] = Node mempty []
-- f (c : cs) = Node c (map (f . (: [])) cs)

treeToList :: Tree a -> [a]
treeToList (Node val subtrees) =
  val : (subtrees >>= treeToList)

-- ran :: Ran Tree [] String
-- ran =
--   let getHerStarted = Node 
--   Ran (\str2tr -> 

main :: IO ()
main = do
  let
    -- _ = f
      _ = Ran
      _  = (undefined :: Ran Tree [] String)
  putStrLn "hello world"

exampleRan :: Ran Tree [] String
exampleRan =
  let a = undefined
      kToD = undefined
  in Ran (\aToTree -> kToD (aToTree a))


-- redefinition to simplify Parsec's type
char :: Char -> Parser Char
char = P.char

-- redefinition to simplify Parsec's type
satisfy :: (Char -> Bool) -> Parser Char
satisfy = P.satisfy

-- redefinition to simplify Parsec's type
string :: String -> Parser String
string = P.string

-- redefinition to simplify Parsec's type
eof :: Parser ()
eof = P.eof

-- redefinition to simplify Parsec's type
parserFail :: String -> Parser a
parserFail = P.parserFail

-- non-greedy version of an alternative combinator
(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (P.try prsr1) prsr2

-- non-greedy version of an choice combinator
choice :: [Parser a] -> Parser a
choice = P.choice . fmap P.try

-- non-greedy version of a sepBy combinator
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy body (P.try sep)

-- non-greedy version of a sepBy1 combinator
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

-- non-greedy version of many
many :: Parser a -> Parser [a]
many = P.many . try

-- non-greedy version of many1
many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

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

-- null parser
jNullP :: Parser JSON
jNullP = pure JNull <* string "null"

-- boolean parser
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

-- float parser
jFloatP :: Parser JSON
jFloatP = let mkFloat (sign, a) b =
                JFloat $ sign * read (a <> "." <> b)
          in pure mkFloat
             <*> digitsWithSignP
             <* char '.'
             <*> many1 digit

-- helper parser for JSON string parser
stringP :: Parser String
stringP = between
      (char '"')
      (char '"')
      (many (satisfy (/= '"')))

-- string parser
jStringP :: Parser JSON
jStringP = pure JString <*> stringP

-- array parser
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

-- function that runs a particular parser
parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

-- function that parses JSON
parseJSON :: String -> Either ParseError JSON
parseJSON = parse inputP

