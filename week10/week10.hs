isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n

truncatablePrime :: Int -> Bool
truncatablePrime 0 = True
truncatablePrime x = isPrime x && truncatablePrime (div x 10)

contain :: Int -> Int -> Bool
contain x y 
 | x == 0 = False
 | mod x 10 == y = True
 | otherwise = contain (div x 10) y

containsDigits :: Int -> Int -> Bool
containsDigits x y
 |y == 0 = True
 |contain x (mod y 10) = containsDigits x (div y 10) 
 |otherwise = False

   
productOfDigits :: Int -> Int
productOfDigits 0 = 1
productOfDigits x = mod x 10 * productOfDigits (div x 10)

sumOfDivisors :: Int -> Int
sumOfDivisors x = sumOfDivisors' x 1
 where
 sumOfDivisors' :: Int -> Int -> Int 
 sumOfDivisors' z y
  | z == y = 0
  | mod z y == 0 = y + sumOfDivisors' z (y + 1)
  | otherwise = sumOfDivisors' z (y + 1)

interestingNumber :: Int -> Bool
interestingNumber x = sumOfDivisors (sumOfDivisors x) == x

quadrant :: Double -> Double -> Int
quadrant 0 0 = 0
quadrant x y 
 |x > 0 && y > 0 = 1
 |x < 0 && y > 0 = 2
 |x < 0 && y < 0 = 3
 |x > 0 && y < 0 = 4
 
