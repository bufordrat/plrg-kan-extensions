{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.Tree
import Data.Char

-- Milewski right Kan extension datatype
newtype Ran k d a = Ran (forall i. (a -> k i) -> d i)

-- example where Tree functor is k
f :: String -> Tree Int
f "" = Node 0 []
f (c : cs) = Node (ord c) (map (f . (: [])) cs)

main :: IO ()
main = do
  let _ = f
      _ = Ran
      _  = (undefined :: Ran Tree [] String)
  putStrLn "hello world"
