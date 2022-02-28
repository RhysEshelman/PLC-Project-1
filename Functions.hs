module Functions where

import Data.Char
import Data.List
import System.Environment
import Control.Monad

net_format :: [(Int,Int)] -> String
net_format [] = ""
net_format (c:cs) = 
         foldl (\x y -> x ++ "\n" ++ (format_help y)) (format_help c) cs

format_help :: (Int, Int) -> String
format_help (x,y) = (show x) ++ " -- " ++ (show y)

par_format :: [[(Int, Int)]] -> String
par_format [] = ""
par_format (c:cs) =
         foldl (\x y -> x ++ "\n" ++ (par_help y)) (par_help c) cs

par_help :: [(Int, Int)] -> String
par_help [] = []
par_help (c:cs) = 
         foldl (\x y -> x ++ " , " ++ (format_help y)) (format_help c) cs
