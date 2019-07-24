module Lab3ab where



----- PART A -----

-- A.1 

{--
data Nat = Zero | Succ Nat 

instance Eq Nat where
	Zero == Zero = True
	Succ Zero == Succ Zero = True
	Succ a == Succ b = a == b
	_ == _ = False
    
    
instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ x) = "Succ " ++ (show x)
--}


-- A.2

data Nat = Zero | Succ Nat deriving (Show, Eq)



-- A.3

instance Ord Nat where
    Zero <= Zero = True
    Zero <= _ = True
    _ <= Zero = False
    Succ a <= Succ b = a <= b


{- Likely, Haskell would have derived the <= properly. It would have 
denoted Zero as less than Succ _ it comes first in the constructor. Then
comparing Succ a and Succ b it would compare their constructor value 
as desired. So we could have simply let Haskell derive it for us.
-}




-- A.4

data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)
  

instance Eq SignedNat where
    Neg Zero == Neg Zero = True
    Pos Zero == Pos Zero = True
    Neg Zero == Pos Zero = True
    Pos Zero == Neg Zero = True
    Pos a == Pos b       = a == b
    Neg a == Neg b       = a == b
    _ == _               = False


instance Ord SignedNat where
    Pos Zero <= Neg Zero = True
    Neg _ <= Pos _ = True
    Pos _ <= Neg _ = False
    Neg a <= Neg b = b <= a
    Pos a <= Pos b = a <= b
    
    
{- It would not have worked to let Haskell to define Ord and Eq for us
for SignedNat. This is because it would have determined that Neg Zero
is strictly less than Pos Zero which is not true for the defintion
that we want for SignedNat. 
-}


-- A.5

absSignedNat :: SignedNat -> SignedNat
absSignedNat (Pos a) = Pos a
absSignedNat (Neg a) = Pos a

negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Pos Zero) = Pos Zero
negateSignedNat (Pos a) = Neg a
negateSignedNat (Neg a) = Pos a






addNat :: Nat -> Nat -> Nat
addNat a Zero = a
addNat Zero a = a
addNat (Succ a) (Succ b) = Succ (Succ (addNat a b))


addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos Zero) a = a
addSignedNat a (Pos Zero) = a
addSignedNat (Neg Zero) a = a
addSignedNat a (Neg Zero) = a
addSignedNat (Pos a) (Pos b) = Pos (addNat a b)
addSignedNat (Pos (Succ a)) (Neg (Succ b)) = addSignedNat (Pos a) (Neg b)
addSignedNat (Neg (Succ b)) (Pos (Succ a)) = addSignedNat (Pos a) (Neg b)
addSignedNat (Neg a) (Neg b) = Neg (addNat a b)


mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Succ m) n = addNat n (mulNat m n)


mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Pos Zero) _ = (Pos Zero)
mulSignedNat _ (Pos Zero) = (Pos Zero)
mulSignedNat (Neg Zero) _ = (Neg Zero)
mulSignedNat _ (Neg Zero) = (Neg Zero)
mulSignedNat (Pos a) (Pos b) = Pos (mulNat a b)
mulSignedNat (Neg a) (Neg b) = Pos (mulNat a b)
mulSignedNat (Pos a) (Neg b) = Neg (mulNat a b)
mulSignedNat (Neg a) (Pos b) = Neg (mulNat a b)


signumSignedNat :: SignedNat -> SignedNat
signumSignedNat (Pos Zero) = (Pos Zero)
signumSignedNat (Neg Zero) = (Neg Zero)
signumSignedNat (Pos _) = (Pos (Succ Zero))
signumSignedNat (Neg _) = (Neg (Succ Zero))


fromIntegerNat :: Integer -> Nat 
fromIntegerNat 0 = Zero
fromIntegerNat a | a > 0 = (Succ (fromIntegerNat (a - 1)))
fromIntegerNat a = (Succ (fromIntegerNat (a + 1)))


fromIntegerSigned :: Integer -> SignedNat
fromIntegerSigned 0 = (Pos Zero)
fromIntegerSigned a | a > 0 = Pos (fromIntegerNat a) 
fromIntegerSigned a = Neg (fromIntegerNat a)


instance Num SignedNat where
  (+) = addSignedNat
  (*) = mulSignedNat
  negate = negateSignedNat
  abs = absSignedNat
  signum = signumSignedNat
  fromInteger = fromIntegerSigned


-- A.6

toInt :: SignedNat -> Integer -> Integer
toInt (Pos Zero) a = a
toInt (Pos (Succ n)) a = toInt (Pos n) (a + 1) 
toInt (Neg Zero) a = a
toInt (Neg (Succ n)) a = toInt (Neg n) (a - 1)


signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger a = toInt a 0


-- A.7 

{-
One of the downsides of the SignedNat datatype was the issue of having
both positive 0 and negative zero. I think it might be useful to have 
a single zero value. It also might be useful to have slightly 
terminology for negative integers so it is less confusing. For example
it's strange to think of -3 as the successor to -2. Here is the change
I propose. 

data PosUnary =
	One | Succ PosUnary

data NegUnary
	negOne | Succ NegUnary

data UnaryInteger
	NegUnary | Zero | PosUnary


I didn't address the non-intuitive construction of negative integers
as described as above. This change helped since we don't have to values
for Zero as before. However, when we rewrite code, we may have to be
careful that we aren't off by one since now our base cases may end
at one rather than 0. We also now have three data declarations now
rather than two which is potentially tougher to work with. We did solve
the issue of only having one zero now which is more representative of 
the actual set of Integers. 
-}


-- A.8

factorial :: (Num a, Ord a) => a -> a
factorial a | a < 0 = error "input must be positive"
            | a == 0 = 1
factorial a = a * factorial(a - 1)




----- PART B --------


-- B.1

{-
The operator must just be infix because it is non-associative. The 
output of the operator is a string and the inputs are integers. 
Therefore, a chained operator expression will have type errors.
-}



-- B.2

{-
The operator can be either infixl or infixr. Both ways will not result
in type errors and both actually give the same answer too. 
For example we look at the expression: 7 +| 6 +| 5
We said the operator is left associative so the parser will interpret
the input as (7 +| 6) +| 5 and evaluate as (7 +| 6) +| 5 = 3 +| 5 = 8
-}



-- B.3

{-
This operator must be infixl. 
For example we look at the expression [1, 2] &< 3 &< 4.
We must have list on the left side otherwise the expression wouldnt
evaluate. If it was infixr the expression wouldnt work because
3 &< 4 would have type errors. Instead, it is infixl so it will
evaluate as
[1, 2] &< 3 &< 4
[1, 2, 3] &< 4
[1,2,3,4]
-}


-- B.4

{-
This operator must be infixr. 
For example we consider the expression 1 >&& 2 >&& [3, 3].
This must be right associative becuse 1 >&& 2 will give type errors
if evaluated first. The expression evaluates as follows:
1 >&& 2 >&& [3, 3]
1 >&& [2, 2, 3, 3]
[1, 1, 2, 2, 3, 3]
-}


-- B Part 2

{-
As far as type issues go, the operator can either be infixl or infixr.
However, its associativity should really just be infix.
Think if we have the expresssion 
                             24 +# 102 +# 85
We likely want to know how many digits the total sum is. However, 
if we do left associative we get
                     (4 +# 102) +# 85 = 3 +# 85 = 2
And with right associative
                     4 +# (102 +# 85) = 4 +# 3 = 1
However, the answer we actually desire is 3. Thus, the answer should be 
infix. 
-}
