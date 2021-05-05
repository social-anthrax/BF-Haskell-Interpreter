module Main where

import Compiler (runBrainFuckFromFile)

main :: IO ()
main = runBrainFuckFromFile "Test.txt"
