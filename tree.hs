-- Andrey Perunov
-- st81049

-- Variant 1-2
--Consider the following datatype definition for binary trees
-- that we shall want to use to implement binary search trees:
-- data Tree a = Branch a (Tree a) (Tree a) | Leaf
-- Write a function isSearchTree :: Tree a −> Bool that verifies that its argument is a binary search tree.

-- Then test the property that given a binary search tree t, inserting a value into the tree results in yet another binary search tree. The code for inserting a new value into the tree is:
-- insertTree :: Ord a => a −> Tree a −> Tree a
-- insertTree e Leaf = Branch e Leaf Leaf
-- insertTree e (Branch x li re)
--    | e <= x = Branch x (insertTree e li) re
--    | e > x = Branch x li (insertTree e re)

-- Experiment with mutating the implementation of insertTree to find out whether your property can in fact discover that the mutated implementation no longer maps binary search trees to binary search trees.

-- Leaf == NIL == NULL
data Tree a = Branch a (Tree a) (Tree a) | Leaf

isSearchTree :: Ord a => Tree a -> Bool
-- if it is NIL (Leaf) then it's binary search tree
isSearchTree Leaf = True
isSearchTree (Branch node leftSubTree rightSubTree) = isLeftSmaller node leftSubTree && isRightLarger node rightSubTree && isSearchTree leftSubTree && isSearchTree rightSubTree
  where
    -- checking that left node is smaller then parent
    isLeftSmaller parentNode Leaf = True
    isLeftSmaller parentNode (Branch node leftSubTree rightSubTree) = parentNode >= node
    -- checking that right node is larger then parent
    isRightLarger parentNode Leaf = True
    isRightLarger parentNode (Branch node leftSubTree rightSubTree) = parentNode < node

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x li re)
  | e <= x = Branch x (insertTree e li) re
  | e > x = Branch x li (insertTree e re)

main :: IO ()
main = do
  let t = Branch 5 (Branch 3 Leaf Leaf) (Branch 7 Leaf Leaf)
  print $ isSearchTree t
  print $ isSearchTree $ insertTree 8 t
  print $ isSearchTree $ insertTree 2 t
  print $ isSearchTree $ insertTree 4 t
  