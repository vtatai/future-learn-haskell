-- sortedtree.hs
-- Jeremy.Singer@glasgow.ac.uk
-- Example code for #FLhaskell course

-- Nodes contain integers, Leaves are empty
data Tree = Leaf | Node Int Tree Tree deriving Show



treeDepth :: Tree -> Int
-- longest path from root to a leaf
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)


isSortedTree :: Tree -> Int -> Int -> Bool
-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted



addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

-- PROPOSED EXERCISES
-- For testing: let tree = Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf (Node 13 Leaf Leaf))
{- Insert in order
    Takes an ordered tree and inserts a new node into it
-}
addNewValue :: Tree -> Int -> Tree
addNewValue Leaf x = Node x Leaf Leaf
addNewValue (Node treeVal l r) x 
  | x == treeVal = Node x Leaf (Node treeVal l r)
  | x < treeVal = Node treeVal (addNewValue l x) r
  | x > treeVal = Node treeVal l (addNewValue r x)

-- Convert to list
inOrderToList :: Tree -> [Int]
inOrderToList Leaf = []
inOrderToList (Node x l r) = (inOrderToList l) ++ [x] ++ (inOrderToList r)

-- Convert to list, pre order, why not
preOrderToList :: Tree -> [Int]
preOrderToList Leaf = []
preOrderToList (Node x l r) = [x] ++ (preOrderToList l) ++ (preOrderToList r)

-- Convert to list, post order
postOrderToList :: Tree -> [Int]
postOrderToList Leaf = []
postOrderToList (Node x l r) = (postOrderToList l) ++ (postOrderToList r) ++ [x]
