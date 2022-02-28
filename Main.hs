module Main where

import Functions
import Data.List
import System.Environment

main :: IO ()
main =
  do
    cargs <- getArgs
    case cargs of
      [] -> fail "Run with one command-line argument"
      command:args ->
        do
          case command of
            "Read" -> 
              do
                inps <- readFile $ head args
                let l = read inps :: [(Int,Int)]
                writeFile "network.txt" (net_format l)
            "Run" -> 
              do
                net <- readFile $ head args
                let network = read net :: [(Int,Int)]
                seq <- read head $ tail args
                putStrLn $ runNetwork network seq
            "Parallel" ->
              do
                inps <- readFile $ head args
                let l = read inps :: [[(Int,Int)]]
                writeFile "parallel.txt" (par_format l)
            "Sorting" -> putStrLn "Unimplemented"
            "Create" -> putStrLn "Unimplemented"
