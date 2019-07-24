import Control.Monad
import Control.Monad.State
import Data.IORef


------ PART A ------

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)


-- A.1

factIO :: Integer -> IO Integer
factIO x | x < 0 = error "invalid arguement; must be non-negative"
factIO x = do
    res <- newIORef 1
    count <- newIORef x
    whileIO
        (do
            counter <- readIORef count
            return (counter /= 0))
        (do
            prev <- readIORef res
            counter <- readIORef count
            writeIORef res (prev * counter)
            writeIORef count (counter - 1))
    readIORef res
        
        
-- A.2

factState :: Integer -> Integer
factState x | x < 0 = error "invalid arguement; must be non-negative"
factState x = evalState helper (x, 1)
    where
        helper :: State (Integer, Integer) Integer
        helper = do
            whileState
                (\(c, _) -> c /= 0)
                (do
                    (c, res) <- get
                    put ((c - 1), (res * c)))
            (_, final) <- get
            return final
                

-- A.3

fibIO :: Integer -> IO Integer
fibIO x | x < 0 = error "invalid arguement; must be non-negative"
fibIO x = do
    f0 <- newIORef 0
    f1 <- newIORef 1
    counter <- newIORef x
    whileIO
        (do 
            c <- readIORef counter 
            return (c /= 0))
        (do
            n1 <- readIORef f0
            n2 <- readIORef f1
            c <- readIORef counter
            writeIORef f0 n2
            writeIORef f1 (n1 + n2)
            writeIORef counter (c - 1))
          
    readIORef f0


-- A.4

fibState :: Integer -> Integer
fibState x | x < 0 = error "invalid arguement; must be non-negative"
fibState x = evalState helper (0, 1, x)
    where 
        helper :: State (Integer, Integer, Integer) Integer
        helper = do
            whileState
                (\(_, _, c) -> c /= 0)
                (do
                    (f0, f1, c) <- get
                    put (f1, f1 + f0, c - 1))
            (res, _, _) <- get
            return res
            
    


------ PART B -------

{-
Given:
runReader :: Reader r a -> r -> a
runReader (Reader f) = f

instance Monad (Reader r) where
  return x = Reader (\r -> x)

  mx >>= f = Reader (\r -> 
               let x = runReader mx r in
                 runReader (f x) r)


We start with >>= operator:

We are given functions f and g where they have type signatures:
f :: a -> Reader r b
g :: b -> Reader r c 
We want to compose these into a new function, h, with 
h :: a -> Reader r c

We define everything non-mondadicaly:
f' :: (a, r) -> b
g' :: (b, r) -> c
h' :: (a, r) -> c

We could define h' as 
h' (x, r) = 
	let y = (f' (x, r)) in
	let z = (g' (y, r)) in
	(z, r)

We have:
h = f >=> g 
h x = f x >=> g x
h x = f x >>= g
f x >>= g = h x

We now simplify this to 
f x >>= g = Reader(\r -> h'(x, r))
Sub in for h'
f x >>= g = Reader(\r -> 
                   let y = (f' (x, r)) in
	               let z = (g' (y, r)) in 
	               (z, r)

f x >>= g = Reader(\r -> 
                   let y = (f' (x, r)) in
	               (g' (y, r))

We have  f x = Reader (\r-> f'(x, r))

so f x >>= g = 
              Reader(\r -> let (Reader ff) = f x 
                           y = ff r 
                           in g' (y, r))

Do the same for g'

f x >>= g = 
              Reader(\r -> let (Reader ff) = f x 
                           y = ff r 
                           (Reader gg) = g y
                          in gg r)
Sub in for fx
mv >>= g = Reader (\r -> 
            let (Reader ff) = mv
                y = ff r
                (Reader gg) = g y
            in gg r)

Substitute to get:
mv >>= g = Reader (\r ->
               let (Reader g) = mv
                   x = g r
                   (Reader h) = f x
               in h r)


Now, we must show the derivation of return.

We do as we did before starting with
return' (x, r) = x
And deriving
return'' x = (\r -> x)
Which gives us
return x = Reader (\r -> x) as desired.

-}
