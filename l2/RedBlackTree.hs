module RedBlackTree where


-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees, and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show


------- PART A -------

-- A.1
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node _ left val right) | x == val = True
                                 | x < val = member x left
                                 | otherwise = member x right 


-- A.2
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ left val right) = [val] ++ toList left ++ toList right



-- A.3


insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t) 
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right) 
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t = 
      Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
      
    balance Black (Node Red l1 x (Node Red l2 y r2)) z r1 = 
      Node Red (Node Black l1 x l2) y (Node Black r2 z r1)
    
    balance Black l1 x (Node Red (Node Red l2 y r2) z r1) =
      Node Red (Node Black l1 x l2) y (Node Black r2 z r1)
      
    balance Black l1 x (Node Red l2 y (Node Red l3 z r1)) =
      Node Red (Node Black l1 x l2) y (Node Black l3 z r1)
    
    balance color l e r = Node color l e r  -- no balancing needed


-- A.4
fromList :: Ord a => [a] -> Tree a
fromList lst = foldr insert Leaf lst

-- A.5
minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ left _ right) = min (1 + minDepth left) (1 + minDepth right)


maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ left _ right) = max (1 + maxDepth left) (1 + maxDepth right)

-- A.6
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ Leaf _ Leaf) = True
testInvariant1 (Node _ Leaf val right@(Node _ _ rval _)) | val < rval =
    testInvariant1 right
testInvariant1 (Node _ left@(Node _ _ lval _) val Leaf) | val > lval =
    testInvariant1 left
testInvariant1 (Node _ left@(Node _ _ lval _) val right@(Node _ _ rval _)) 
    | val > lval && val < rval = testInvariant1 left && testInvariant1 right
testInvariant1 _ = False


-- A.7
testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node _ Leaf _ Leaf) = True
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False
testInvariant2 (Node Red _ _ (Node Red _ _ _)) = False
testInvariant2 (Node Red left@(Node Black _ _ _) _ right@(Node Black _ _ _))
    = testInvariant2 left && testInvariant2 right
testInvariant2 (Node Black left _ right) = testInvariant2 left && testInvariant2 right
testInvariant2 _ = True


-- A.8
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = 
        leafCounts left (n+1) ++ leafCounts right (n+1)
    leafCounts (Node Red left _ right) n = 
        (leafCounts left n) ++ (leafCounts right n)

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False



------- PART B -------


-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList


-- B.1
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset x y = all (\a -> member a y) (toList x)


-- B.2
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet x y = (isSubset x y) && (isSubset y x)


-- B.3
union :: Ord a => Set a -> Set a -> Set a
union x y = foldr insert y (toList x)

-- B.4
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = 
    foldr (\x r -> if member x s2 then insert x r else r) empty (toList s1)
    
-- B.5
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if member x s2 then r else insert x r) empty (toList s1)





