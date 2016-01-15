--zad1
import Data.List

reverseNumber:: Int -> Int
reverseNumber x = reverseNumber' x 0
 where
 reverseNumber':: Int -> Int -> Int
 reverseNumber' 0 res = res
 reverseNumber' x res = reverseNumber' (div x 10) ( (mod x 10) + res * 10)

sumOfDigits = sum . toList

toList :: Int -> [Int] --reversed to list
toList 0 = []
toList x = (mod x 10) : toList (div x 10)

pair :: Int -> Bool
pair x = mod x 2 == 0

impair :: Int -> Bool
impair x = mod x 2 /= 0

isReversibleNumber :: Int -> Bool
isReversibleNumber x = (mod x 10) /= 0 && length (filter pair (toList y)) == 0 
 where 
 y = x + reverseNumber x

reversibleNumbers :: Int -> [Int]
reversibleNumbers x = filter isReversibleNumber [1..x]

--zad2
--(Eq t, Num t) => [t] -> (t,t)
consecutiveElements [] = (-1,-1)
consecutiveElements (x:[]) = (-1,-1)
consecutiveElements (x:y:xs) 
	|x == y - 1 = (x,y)
	|otherwise = consecutiveElements (y:xs)

eliminate :: [Int] -> [Int]
eliminate [] = []
eliminate l 
 | (x,y) == (-1,-1) = [] 
 | otherwise = take x l ++ drop (x + 2) l
 where
 m = maximum l
 a = elemIndices m l 
 (x,y) = consecutiveElements a

countEliminations :: [Int] -> Int
countEliminations = countEliminations' 0 
 where  
 countEliminations' res l 
  | res == 0 && null (eliminate l)  = 0
  | null l = res
  | otherwise = countEliminations' (res + 1) (eliminate l)

--zad3
divisors ::Int -> [Int]
divisors n = [x | x <- [2..n] , (mod n x) == 0]

numbersLower :: Int -> Int ->Bool
numbersLower k n = null $ filter (>k)(toList n)

firstNumber :: [Int] -> Int
firstNumber [] = 0
firstNumber l = head l

smallDigits :: Int -> Int -> Int
smallDigits n k =sum $ map (firstNumber.filter (numbersLower k) . divisors) [2..n]

--zad5
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show
fullTree = Node 1
        (Node 2
            (Node 3 Empty Empty)
            (Node 4 Empty Empty))
        (Node 5 Empty Empty)
nonFullTree = Node 1
            (Node 2
                (Node 3 Empty Empty)
                (Node 4 Empty Empty))
            Empty

isFull :: Tree a -> Bool
isFull Empty = False
isFull (Node _ Empty Empty) = True
isFull (Node _ b c) = isFull b && isFull c


 --zad6
isValid :: Int -> Bool
isValid l = mod (sum1 + sum2 ) 10 == 0
 where
  num = toList l
  len = (length num) - 1
  num1 = zip [0..] num
  sum1 = sum (map snd (filter (pair . fst) num1))
  sum2 = sum $ map (sumOfDigits .(*2).snd) (filter (impair . fst) num1)
