module Lib where

import Data.Tree
import Data.List (isPrefixOf)

dict :: [String]
dict = ["we","wee","weed","eat","eater","eaters","ed","at","ate"]

nextColumn :: [String] -> String -> [String]
nextColumn dict str = filter (flip isPrefixOf str) dict

generateTree :: [String] -> String -> String -> Tree String  
generateTree dict input prefix =
  let newInput = drop (length prefix) input
      eachSubtree = generateTree dict newInput
      column = nextColumn dict newInput
  in Node prefix (map eachSubtree column)

generateLists :: Tree String -> [[String]]
generateLists (Node v []) = [[v]]
generateLists (Node v subtrees) =
  liftA2 (++) [[v]] (subtrees >>= generateLists)

subwords :: [String] -> String -> [[String]]
subwords dict input =
  let (Node _ subtrees) = generateTree dict input ""
  in subtrees >>= generateLists

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . drawTree . fmap show

msg :: String
msg = "Hello, MPCS 51400 World!"
