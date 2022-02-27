module Comparators where
import Data.List

-- Call comparator for part 3
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

-- Call isSortingNetwork for part 5
isSortingNetwork :: [(Int,Int)] -> Bool
isSortingNetwork l
    | null l = True
    | otherwise =
        let xs = [1..(maximum (tupleToList l))]
        in all (==True) (checkSort l (permutations xs))

addLayer :: Int -> Int -> [(Int,Int)]
addLayer i j =
    if i == j then [(i,i+1)]
    else (j,j+1):addLayer i (j+2)

goingUp :: Int -> Int -> [(Int, Int)]
goingUp m n
    | m == n-1 && odd m = addLayer m 1 ++ goingDown (n-2)
    | m == n-1 && even m = addLayer m 2 ++ goingDown (n-2)
    | odd m = addLayer m 1 ++ goingUp (m+1) n
    | otherwise = addLayer m 2 ++ goingUp(m+1) n

goingDown :: Int -> [(Int,Int)]
goingDown m
    | m == 1 = addLayer 1 1
    | odd m = addLayer m 1 ++ goingDown (m-1)
    | otherwise = addLayer m 2 ++ goingDown (m-1)

-- Call computeNetwork along with whatever we used for parallelism in part 4 for part 6
computeNetwork :: Int -> [(Int,Int)]
computeNetwork n
    | n <= 1 = []
    | n == 2 = [(1,2)]
    | otherwise = goingUp 1 n
