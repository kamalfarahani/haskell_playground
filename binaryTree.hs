module BinaryTree where

data BinaryTree a = 
                   Leaf | 
                   Node (BinaryTree a) a (BinaryTree a)
                   deriving(Eq, Ord, Show)

data TreeOrder = Pre | In | Post
                 deriving (Eq, Show)

insertToTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertToTree value Leaf = Node Leaf value Leaf
insertToTree value (Node left value' right)
    | value < value' = Node (insertToTree value left) value' right
    | value == value' = Node left value right
    | value > value' = Node left value' (insertToTree value right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left value right) = 
    Node (mapTree f left) (f value) (mapTree f right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left value right) =
    foldTree f (f value (foldTree f acc left)) right

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f tree = foldTree  (\a b -> Node b (f a) Leaf) Leaf tree

treeToList :: TreeOrder -> BinaryTree a -> [a]
treeToList _ Leaf = []
treeToList order (Node left value right) = 
    case order of
    Pre -> [value] ++ (f left) ++ (f right) 
    In -> (f left) ++ [value] ++ (f right)
    Post -> (f left) ++ (f right) ++ [value]
    where f = treeToList order

preorder :: BinaryTree a -> [a]
preorder = treeToList Pre

inorder :: BinaryTree a -> [a]
inorder = treeToList In

postorder :: BinaryTree a -> [a]
postorder = treeToList Post