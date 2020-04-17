timesTen :: Int -> Int
timesTen x = 10 * x

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle x = pi * x^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder x y = (areaOfCircle x)  * y

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 =  sqrt((y1 - y2)^2 + (x1 - x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = (x /= y) && (x /= z) && (y /= z)

divisibleBy :: Int-> Int -> Bool
divisibleBy x y = if (mod x y == 0) then True else False

isEven :: Int -> Bool
isEven x = even x

averageThree ::Int -> Int -> Int -> Float
averageThree x y z = fromIntegral(x + y + z)/ 3

absolute :: Int -> Int
absolute x = if x >= 0 then x else -x
