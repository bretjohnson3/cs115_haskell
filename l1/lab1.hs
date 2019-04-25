-- PART B

-- B.1

(+*) :: Double -> Double -> Double
(+*) x y = (x * x) + (y * y)
infixl 7 +*

(^||) :: Bool -> Bool -> Bool
(^||) False y = y
(^||) True y = not y
infixr 3 ^||

-- B.2 

rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | y < x = error ("First argument greater than second")
 				 | x == y = x
 				 | otherwise = x * (rangeProduct (x + 1) y)


-- B.3 

prod :: [Integer] -> Integer
prod = foldr (*) 1


-- B.4

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2  _ [] _ = []
map2 _ _ [] = []
map2 f (a:as) (b:bs) = f a b : map2 f as bs 


map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (a:as) (b:bs) (c:cs) = f a b c : map3 f as bs cs
 
{-- 

dot :: [Integer] -> [Integer] -> Integer
dot = (sum .) . map2 (*)

dot lst1 lst2
(sum .) . map2 (*) lst1 lst2
(sum .) (map2 (*) lst1 lst2)
(\x -> sum . x) (map2 (*) lst1 lst2)
sum map2 (*) lst1 lst2

--}



-- B.5 

res = sum [x | x <- [1 .. 999], mod x 5 == 0 || mod x 3 == 0]

-- the result is 233168


-- B.6

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

primes = sieve [x | x <- [2 ..]]
res1 = sum (takeWhile (<10000) primes)

--res1 = 5736396




-- PART C

-- C.1
{--
sumList :: [Integer] -> Integer
sumList [] = 0
sumList lst = head lst + sumList (tail lst)

shouldn't use head and tail here. Should pattern match instead
Corrected code should be:
--}

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList (xs)

-- C.2
{--
-- Return the largest value in a list of integers.
largest :: [Integer] -> Integer
largest xs | length xs == 0 = error "empty list"
largest xs | length xs == 1 = head xs
largest xs = max (head xs) (largest (tail xs))

Again, we shouldn't use head and tail. Also, no need 
to have pattern guards on the empty list and single element
list cases; use pattern matching instead.
--}
-- Return the largest value in a list of integers.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest (x:[]) = x
largest (x:xs) = max (x) (largest (xs))



-- PART D

-- D.1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{-- fib 3

--> fib (3 - 1) + fib (3 - 2)
--> fib (2) + fib (3 - 2)
--> fib (2 - 1) + fib (2 - 2) + fib (3 - 2)
--> fib (1) + fib (2 - 2) + fib (3 - 2)
--> 1 + fib (2 - 2) + fib (3 - 2)
--> 1 + fib (0) + fib (3 - 2)
--> 1 + 0 + fib (1) 
--> 1 + 0 + 1
--> 2
--}

-- D.2 
fact :: Integer -> Integer
fact n = n * fact (n - 1)
fact 0 = 1

{--
fact 3
--> 3 * fact (3 - 1)
--> 3 * fact (2)
--> 3 * 2 * fact(1)
--> 3 * 2 * 1 * fact(0)
--> 3 * 2 * 1 * 0 * fact (-1)
--> 3 * 2 * 1 * 0 * fact (-2)

The pattern matching cases need to be reversed. Pattern matching works
such that the first valid case is the one that gets used. Since the 
first pattern match will work on any number, it is used every time. 
Thus, this factorial function will not terminate.
--}

-- D.3

reverse :: [a] -> [a]
reverse xs = iter xs []
  where
    iter :: [a] -> [a] -> [a]
    iter [] ys = ys
    iter (x:xs) ys = iter xs (x:ys)
    
{-- 
reverse [1, 2, 3]
--> iter [1, 2, 3] []
--> iter [2, 3] (1 :: [])
--> iter [3] (2 :: (1 :: []))
--> iter [] (3 :: (2 :: (1 :: [])))
--> (3 :: (2 :: (1 :: [])))
--> [3, 2, 1]

The complexity of this is O(n). We iterate through the list once
each time pulling off the first element
--}

-- D.4
{--
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
For reference: The definition of the (++) operator is:

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)


reverse [1, 2, 3]
--> (reverse [2, 3]) ++ [1]
--> ((reverse [3]) ++ [2]) ++ [1]
--> (((reverse []) ++ [3]) ++ [2]) ++ [1]
--> (([] ++ [3]) ++ [2]) ++ [1]
--> [3] ++ [2] ++ [1]
--> (3: ([] ++ [2])) ++ [1]
--> (3 : [2]) ++ [1]
--> [3, 2] ++ [1]
--> 3: ([2] ++ [1])
--> 3: (2: ([] ++ 1))
--> 3: (2: [1])
--> [3, 2, 1]

The issue is with the concatenations. They each happen in linear time
with respect to the size of the input. However, the inputs to all the
concatenations are singletons. Thus, we do n concatenations which are
all of O(n) resulting in O(n^2).
--}

-- D.5

{--

isort :: [Integer] -> [Integer]
isort [] = []
isort (n:ns) = insert n (isort ns)
  where
    insert :: Integer -> [Integer] -> [Integer]
    insert n [] = [n]
    insert n m@(m1:_) | n < m1 = n : m
    insert n (m1:ms) = m1 : insert n ms

head (isort [3, 1, 2, 5, 4])
--> head (insert 3 (isort [1, 2, 5, 4])
--> head (insert 3 (insert 1 (isort [2, 5, 4])))
--> head (insert 3 (insert 1 (isort [2, 5, 4])))
--> head (insert 3 (insert 1 (insert 2 isort [5, 4])))
--> head (insert 3 (insert 1 (insert 2 (insert 5 isort [4]))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 isort [])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
--> head (insert 3 (insert 1 (insert 2 (4: insert 5 [])))
--> head (insert 3 (insert 1 (insert 2 (4: [5])))
--> head (insert 3 (insert 1 (insert 2 [4, 5]))
--> head (insert 3 (insert 1 ([2, 4, 5]))
--> head (insert 3 ([1, 2, 4, 5]))
--> head (1 : insert 3 [1, 2, 4, 5])
--> 1


-- D.6

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init 
foldr f init (x:xs) = f x (foldr f init xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x:xs) = foldl f (f init x) xs

1. foldr max 0 [1, 5, 3, -2, 4]
--> max 1 (foldr max 0 [5, 3, -2, 4])
--> max 1 (max 5 (foldr max 0 [3, -2, 4]))
--> max 1 (max 5 (max 3 foldr max 0 [-2, 4]))
--> max 1 (max 5 (max 3 (max -2 foldr max 0 [4])))
--> max 1 (max 5 (max 3 (max -2 (max 4 foldr max 0 []))))
--> max 1 (max 5 (max 3 (max -2 (max 4 0))))
--> max 1 (max 5 (max 3 (max -2 4)))
--> max 1 (max 5 (max 3 4))
--> max 1 (max 5 4)
--> max 1 5
--> 5


2. foldl max 0 [1, 5, 3, -2, 4]

--> foldl max 0 [1, 5, 3, -2, 4]
--> foldl max (max 0 1) [5, 3, -2, 4]
--> foldl max (max (max 0 1) 5) [3, -2, 4]
--> foldl max (max (max (max 0 1) 5) 3) [-2, 4]
--> foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
--> foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) [])
--> (max (max (max (max (max 0 1) 5) 3) -2) 4)
--> (max (max (max (max 1 5) 3) -2) 4)
--> (max (max (max 5 3) -2) 4)
--> (max (max 5 -2) 4)
--> (max 5 4)
--> 5

--}

