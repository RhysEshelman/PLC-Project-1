module Main where

import Functions
import Comparators
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
                let seq = read (head $ tail args) :: [Int]
                putStrLn $ show $ comparator network seq
            "Parallel" ->
              do
                inps <- readFile $ head args
                let l = read inps :: [[(Int,Int)]]
                writeFile "parallel.txt" (par_format l)
            "Sorting" -> 
              do
                inps <- readFile $ head args
                let network = read inps :: [(Int,Int)]
                putStrLn $ show $ isSortingNetwork network
            "Create" -> 
              do
                let n = read (head args) :: Int
                let network = computeNetwork n
                putStrLn "Unfinished"
                -- writeFile "parallel.txt" (par_format network)
