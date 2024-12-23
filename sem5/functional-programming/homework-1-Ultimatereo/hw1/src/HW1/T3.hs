module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Meta = M Int Int
  deriving (Show)

-- AVL-tree
data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l value r =
  Branch
    ( M
        (tsize l + tsize r + 1)
        (max (tdepth l) (tdepth r) + 1)
    )
    l
    value
    r

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (M size _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (M _ depth) _ _ _) = depth

tmember :: (Ord a) => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l value r)
  | x < value = tmember x l
  | x > value = tmember x r
  | otherwise = True

tFromList :: (Ord a) => [a] -> Tree a
tFromList = foldr tinsert Leaf

tinsert :: (Ord a) => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x tree@(Branch _ l value r)
  | x < value = balance (mkBranch (tinsert x l) value r)
  | x > value = balance (mkBranch l value (tinsert x r))
  | otherwise = tree

-- Difference between left and right subtree
diff :: (Ord a) => Tree a -> Int
diff Leaf = 0
diff (Branch _ l _ r) = tdepth l - tdepth r

balance :: (Ord a) => Tree a -> Tree a
-- Small Left rotation
balance treeA@(Branch _ treeP a treeB@(Branch _ treeQ b treeR))
  | diff treeA == -2 && (diff treeB == -1 || diff treeB == 0) =
      Branch
        (M (tsize treeA) (tdepth treeQ + 2))
        ( Branch
            (M (tsize treeQ + tsize treeP + 1) (tdepth treeQ + 1))
            treeP
            a
            treeQ
        )
        b
        treeR
-- Big left rotation
balance treeA@(Branch _ treeP a treeB@(Branch _ treeC@(Branch _ treeQ c treeR) b treeS))
  | diff treeA == -2 && diff treeB == 1 && (diff treeC >= -1 || diff treeC <= 1) =
      Branch
        (M (tsize treeA) (tdepth treeA - 1))
        ( Branch
            (M (tsize treeP + tsize treeQ + 1) (tdepth treeA - 2))
            treeP
            a
            treeQ
        )
        c
        ( Branch
            (M (tsize treeR + tsize treeS + 1) (tdepth treeA - 2))
            treeR
            b
            treeS
        )
-- Small Right rotation
balance treeA@(Branch _ treeB@(Branch _ treeP b treeQ) a treeR)
  | diff treeA == 2 && (diff treeB == 1 || diff treeB == 0) =
      Branch
        (M (tsize treeA) (tdepth treeQ + 2))
        treeP
        b
        ( Branch
            (M (tsize treeQ + tsize treeR + 1) (tdepth treeQ + 1))
            treeQ
            a
            treeR
        )
-- Big Right rotation
balance treeA@(Branch _ treeB@(Branch _ treeP b treeC@(Branch _ treeQ c treeR)) a treeS)
  | diff treeA == 2 && diff treeB == -1 && (diff treeC >= -1 || diff treeC <= 1) =
      Branch
        (M (tsize treeA) (tdepth treeA - 1))
        ( Branch
            (M (tsize treeP + tsize treeQ + 1) (tdepth treeA - 2))
            treeP
            b
            treeQ
        )
        c
        ( Branch
            (M (tsize treeR + tsize treeS + 1) (tdepth treeA - 2))
            treeR
            a
            treeS
        )
balance tree = tree
