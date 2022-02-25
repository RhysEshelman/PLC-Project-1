module Comparators where
import Data.List

comparator :: Ord a => [(Int,Int)] -> [a] -> [a]
comparator [] l = l
comparator ((x,y):xs) l =
    if l !! (x-1) <= l !! (y-1) then comparator xs l
    else
        let elemX = [l !! (x-1)]
            elemY = [l !! (y-1)]
            left = take (x-1) l
            middle = take (y - x - 1) (drop x l)
            right = drop y l
        in comparator xs (left ++ elemY ++ middle ++ elemX ++ right)

tupleToList :: [(a,a)] -> [a]
tupleToList [] = []
tupleToList ((x,y):xs) = x : y : tupleToList xs

checkSort :: Ord a => [(Int,Int)] -> [[a]] -> [Bool]
checkSort l = map (\ x -> comparator l x == sort x)

isSortingNetwork :: [(Int,Int)] -> Bool
isSortingNetwork l =
    let xs = [1..(maximum (tupleToList l))]
    in all (==True) (checkSort l (permutations xs))