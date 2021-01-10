module BinaryTree where

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

binInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
binInsert b Leaf = Node Leaf b Leaf
binInsert b (Node left val right)
  | b == val = Node left val right
  | b < val  = Node (binInsert b left) val right
  | b > val  = Node left val (binInsert b right)

binMap :: (a -> b) -> BinaryTree a -> BinaryTree b
binMap f Leaf = Leaf
binMap f (Node left val right) = Node (binMap f left) (f val) (binMap f right)

binPreorder :: BinaryTree a -> [] a
binPreorder Leaf = []
binPreorder (Node left val right) = val : (binPreorder left) ++ (binPreorder right)

binInorder :: BinaryTree a -> [] a
binInorder Leaf = []
binInorder (Node left val right) = (binPreorder left) ++ [val] ++ (binPreorder right)

binPostorder :: BinaryTree a -> [] a
binPostorder Leaf = []
binPostorder (Node left val right) = (binPreorder left) ++ (binPreorder right) ++ [val]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node left val right) =
  foldTree f (foldTree f (f val acc) left) right
-- Note: Folding a binary tree isn't straight forward. With a simple fold over
-- a list all you need to do is combine one element with the result of the next
-- With a binary tree you need to decide how your going to combine the function
-- over the current node, then the left node and then the right node. My
-- intuition is that there are two steps, mapping a current value and then
-- combining it with the mapped values of child values. 
