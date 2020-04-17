absolute :: Int -> Int
absolute x
    | x >= 0 = x
    | otherwise = -x


sign :: Int-> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    |otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z = 3
    | x == y || y == z || x == z = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagx + diagy + diagz
    where
        diagx = x * sqrt 2
        diagy = y * sqrt 2
        diagz = z * sqrt 2

taxiFare :: Int -> Float
taxiFare x
    | x < 10 = fare + first10
    | otherwise = fare + first10 + rest
    where
        fare = 2.20
        first10 = fromIntegral(x) * 0.50
        y = x -10
        rest = fromIntegral(y) * 0.30


howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    | x > average = 1
    | y > average = 1
    | z > average = 1
    where
        average = div (x + y + z) 3


validDate :: Int -> Int -> Bool
validDate x y
    | x > 31 && x <= 0  = False
    | y > 12 && y <= 0 = False
    | y == 2 && x > 28 = False
    | otherwise = True

daysInMonth :: Int -> Int -> Int
daysInMonth x y
    | x == 1 || x == 3 || x == 5 || x == 7 || x == 8 || x == 10 || x == 12 = 31
    | y /= 4 && x == 2 = 29
    |otherwise = 30
