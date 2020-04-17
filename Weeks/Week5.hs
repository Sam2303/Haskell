{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []



--1
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:xs) = x + 1


--2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:(x:xs)


--3
rotate :: [a] -> [a]
rotate (y:(x:xs)) = x:y:xs
rotate xs = xs

--4
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

--5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

--6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) =  x == andAll xs

--7
countElems :: Int -> [Int] -> Int
countElems n [] = 0
countElems n (x:xs)
    | n == x = 1 + countElems n xs
    | otherwise =  countElems n xs


--8
removeAll :: Int -> [Int] -> [Int]
removeAll _  [] = []
removeAll n (x:xs)
    | n == x = removeAll n xs
    | otherwise = x: removeAll n xs


--9
type StudentMark = (String, Int)

listMarks :: String -> [StudentMark] -> [Int]
listMarks st [] = []
listMarks st ((name,mark):stmks)
    | st == name = mark : listMarks st stmks
    | otherwise = listMarks st stmks


--10
sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs)
    |x <= y && sorted (y:xs) = True
    |otherwise = False


prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = True
prefix (x:xs) (y:ys)
    | x /= y = False
    | otherwise = prefix xs ys
