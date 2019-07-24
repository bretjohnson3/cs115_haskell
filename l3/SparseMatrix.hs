------- PART C ------


module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
    SM { bounds :: (Integer, Integer),  -- number of rows, columns
     rowIndices :: S.Set Integer,       -- row indices with nonzeros
     colIndices :: S.Set Integer,       -- column indices with nonzeros
     vals       :: (M.Map (Integer, Integer) a) }  -- values
    deriving (Eq, Show)




-- C.1

-- helper function to get tuples where value isn't zero
getValidTuples :: (Eq a, Num a) => [((Integer, Integer), a)] -> [((Integer, Integer), a)]
getValidTuples lst = filter (\((_, _), a) -> a /= 0) lst

-- using output of getValidTuples, get the first part of the tuple 
-- i.e. the (row, col) pair
getValidCoords :: (Eq a, Num a) => [((Integer, Integer), a)] -> [(Integer, Integer)]
getValidCoords lst = map fst lst



sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
  
sparseMatrix [] bounds = (SM bounds S.empty S.empty M.empty)
sparseMatrix vals (b1, b2)  
    | b1 < 1 || b2 < 1 = error "bounds must be positive" 
    | all (\((r, c), _) -> r <= b1 && c <= b2) vals == False =
              error "some points out of matrix bounds"
sparseMatrix vals bounds =
    -- fill arguements to constructor using helper functions
    SM bounds (S.fromList (map fst coords)) 
        (S.fromList (map snd coords)) (M.fromList tuples)
            where
                tuples = getValidTuples vals
                coords = getValidCoords tuples


-- C.2 

-- add function
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM b1 _ _ _) (SM b2 _ _ _) | b1 /= b2 = error "bounds are not equal"
addSM (SM bounds _ _ map1) (SM _ _ _ map2) = 
    let added_mat = M.toList (M.unionWith (+) map1 map2) in
        sparseMatrix added_mat bounds


-- C.3 

-- negate function
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a 
negateSM (SM a b c map1) = SM a b c (M.map (negate) map1)



-- C.4 

-- subtraction defined in terms of add and negate
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM sm1 sm2 = addSM (sm1) (negateSM sm2)




-- C.5

-- multiplies row vector of first matrix by column vector of second
vectorMul :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a 
             -> Integer -> Integer -> a
vectorMul (SM _ _ _ map1) (SM _ _ _ map2) row col = 
    let rowTuples = M.filterWithKey (\(r, _) _ -> r == row) map1
        rowVals = M.mapKeys snd rowTuples
        
        colTuples = M.filterWithKey (\(_, c) _ -> c == col) map2
        colVals = M.mapKeys fst colTuples in
        -- actually multiply valid entries and sum for vector product
        sum(M.elems (M.intersectionWith (*) rowVals colVals))
        

mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM (_, c1) _ _ _) (SM (r2, _) _ _ _) 
    | c1 /= r2 = error "invalid matrix bounds"
mulSM sm1@(SM (r1, _) rows _ _) sm2@(SM (_, c2) _ cols _) = 
    -- construct new matrix by iteratively multiplying rows with columns
    (sparseMatrix 
    [((x1,x2), vectorMul sm1 sm2 x1 x2) | x1 <- S.toList rows, x2 <- S.toList cols] 
    (r1, c2))


-- C.6

-- retrieves matrix value
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM _ _ _ map1) loc = M.findWithDefault 0 loc map1

-- retrieves row bound
rowsSM :: SparseMatrix a -> Integer
rowsSM (SM(rows, _) _ _ _) = rows

-- retrieves column bound
colsSM :: SparseMatrix a -> Integer
colsSM (SM(_, cols) _ _ _) = cols



-- C.7

(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM




-- C.8 

{- 
For it to be in the Num type class, it must have functions for 
+, -, *, negate, signum, abs, and fromInteger. We have several of these
functions but for signum and fromInteger (and even abs) it isn't 
obvious what these functions would do.
-}









