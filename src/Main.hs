module Main
  ( main
  ) where

import Chapter4.Chapter4Functions
import Chapter2.SimpleFunctions

main :: IO ()
main = do
  print (classifyClients clients)
