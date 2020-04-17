import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)


sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

grade :: StudentMark -> Char
grade (s1,mk)
    | mk >= 70 = 'A'
    | mk >= 60 = 'B'
    | mk >= 50 = 'C'
    | mk >= 40 = 'D'
    | otherwise = 'F'


capMark :: StudentMark -> StudentMark
capMark (st,mk)
    | mk >= 40      = (st,40)
    | otherwise     = (st,mk)


firstNumbers :: Int -> [Int]
firstNumbers x = [x, x - 1 .. 1]

firstSquares :: Int -> [Int]
firstSquares x = [ (x - n)^2 | n <- [x-1,x-2..0]]

capitalise :: String -> String
capitalise x = [toUpper i | i <- x]


onlyDigits :: String -> String
onlyDigits digit = [i | i <- digit, isDigit i]

capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [ capMark(st,mk) | (st,mk) <- stmks ]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents stList = [ (st,grade (st,mk)) | (st,mk) <- stList ]


duplicate :: String -> Int -> String
duplicate x y
    | y == 1 = x
    | y > 1 = x ++ duplicate x (y-1)


divisors :: Int -> [Int]
divisors x
    | x <= 0 = []
    | otherwise = [i | i <- [1 .. x], (mod x i == 0)]


isPrime :: Int -> Bool
isPrime n
    | divisors n == [1,n] = True
    | otherwise = False
