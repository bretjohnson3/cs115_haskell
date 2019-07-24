import Control.Monad


----- PART A -----


-- A.1 

-- Ben's code
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions =
  [((i, l), (j, k), i^3 + l^3) | 
   i <- [1..], 
   j <- [1..i-1], 
   k <- [1..j-1], 
   l <- [1..k-1], 
   i^3 + l^3 == j^3 + k^3]

-- Using list Monad
hr_solutions_list :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions_list = do
    i <- [1..]
    j <- [1..i-1] 
    k <- [1..j-1] 
    l <- [1..k-1] 
    guard  (i^3 + l^3 == k^3 + j^3)
    return ((i,l), (j,k), i^3 + l^3)

    

-- A.2

sum_of_nats :: Integer
sum_of_nats = sum $ do
    i <- [1..1000]
    if i `mod` 3 == 0 || i `mod` 5 == 0
        then return i
        else mzero
    
sum_of_nats_guard :: Integer
sum_of_nats_guard = sum $ do
    i <- [1..1000]
    guard (i `mod` 3 == 0 || i `mod` 5 == 0)
    return i



-- A.3

isPalindrome :: Integer -> Bool
isPalindrome x = show x == reverse (show x)


largestPalindrome :: Integer
largestPalindrome = maximum $ do
    i <- [100..999]
    j <- [100..999]
    guard (isPalindrome (i*j))
    return (i * j)

-- largest is 906609

-- A.4 

type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

exprs = do
    o1 <- ops
    o2 <- ops
    o3 <- ops
    o4 <- ops
    o5 <- ops
    o6 <- ops
    o7 <- ops
    o8 <- ops
    return [N 1, o1, N 2, o2, N 3, o3, N 4, o4, N 5, o5, N 6, o6, N 7, o7, 
        N 8, o8, N 9]
    
normalize :: Expr -> Expr
normalize [] = []
normalize ((N i) : []) = [N i]
normalize ((N i) : (O Cat) : (N j) : rest) = normalize (N ((10 * i) + j):rest)
normalize ((N i) : (O oper) : rest) = N i : O oper : normalize rest
normalize _ = error "not a valid expression!"


evaluate :: Expr -> Int
evaluate [] = 0
evaluate ([N i]) = i
evaluate ((N i) : (O Add) : (N j) : rest) = evaluate (N (i + j) : rest)
evaluate ((N i) : (O Sub) : (N j) : rest) = evaluate (N (i - j) : rest)
evaluate _ = error "Invalid input expression to evaluate"


-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs



------ PART B -------

{-
-- B.1

do n1 <- [1..6]
   n2 <- [1..6]
   []
   return (n1, n2)
   

We desugar:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >>= \_ -> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> [(n1, n2)]) []

This last call has us calling concatMap on the empty list which
will just result in the empty list 




-- B.2

do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)
   
--why do these return the same thing as this expression:

do n1 <- [1..6]
   n2 <- [1..6]
   return (n1, n2)


We desugar:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >>= \_ -> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> 
        concatMap (\_ -> return (n1, n2)) return <anything>
[1..6] >>= \n1 -> [1..6] >>= \n2 -> 
        concatMap (\_ -> [(n1, n2)]) [<anything>]

We see that no matter what the anything list is, we map each element
to the (n1, n2) pairs. So we can simplify to:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [(n1, n2)]

We desugar the second to:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [(n1, n2)]



-- B.3
      
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
     return [c1, c2]
     

We desugar:

s >>= \n -> case n of
    ('a','a', c2, c2,'b','b') -> return [c1, c2]
    _ -> fail "pattern error"

["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>= \n -> case n of
    ('a','a', c2, c2,'b','b') -> return [c1, c2]
    _ -> fail "pattern error"

The pattern match succeeds when the first two letters are 'a's and the 
last two letters are 'b's. That is why we get:
["xy", "zw", "cc"].
If the error was the normal kind and not the list monad, then the 
function would halt once a fail pattern match happened rather
than just return the empty list and continue the computation.



-- B.4

 
We desguar both:

foldr ((++) . k) [] m 
**using m = [x1, x2, ...] gives:
foldr ((++) . k) [] [x1, x2, ...]
foldr (\x -> (++) (k x)) [] [x1, x2, ...]
[k x1, k x2, k x3, ...]


concat (map k m) 
**using m = [x1, x2, ...] gives:
concat (map k [x1, x2 ..])
concat ([(k x1), (k x2) , ..]) 
(k x1) ++ (k x2) ++ .. ++ [] 
[(k x1), (k x2), ... ]

They give the same result!




-- B.5


The error message signals that the error occurs at 
" AnyNum (n + s) " with the second arguement of the + operator. The
error says that the types are not compatible and thus the + operation
cannot be performed. The issue is that AnyNum only needs the arguements
to be the same type CLASS but the + operator needs the arguements to
be the same exact type. There is no way to fix this without changing 
the AnyNum datatype or maybe by writing code with extensive type 
checking.

-}
