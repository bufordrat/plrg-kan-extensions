module Lib where

import Data.Tree

-- Milewski right Kan extension datatype
newtype Ran k d a = Ran { unRan :: forall i. (a -> k i) -> d i }

exampleRan :: a -> Ran Tree [] a
exampleRan a = Ran (\f -> flatten (f a))

intToTree :: Int -> Tree Int
intToTree n =
  let coreducer n | n <= 0 = (0, [])
      coreducer n = (n, n - 1 : [n - 2])
  in unfoldTree coreducer n

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . drawTree . fmap show

