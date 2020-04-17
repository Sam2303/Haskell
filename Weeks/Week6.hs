{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

--1
mult10 :: [Int] -> [Int]
mult10 [] = []
mult10 (x:xs) = 10 * x : mult10 xs

--2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

--3
orAll :: [Bool] -> Bool
orAll = foldr (||) False


--4
sumSquares :: [Int] -> Int
sumSquares [] = 0
sumSquares (x:xs) = x^2 + sumSquares xs

--5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>=0) . filter (<=10)

--6
-- squareRoots :: [Float] -> [Float]
-- squareRoots
