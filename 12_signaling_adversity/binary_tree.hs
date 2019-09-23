module BinaryTree where


data BinaryTree a =
    Leaf |
    Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfoldBinaryTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldBinaryTree f x =
    case (f x) of
        Nothing -> Leaf
        (Just (x1, y, x2)) -> 
            Node (unfoldBinaryTree f x1) y (unfoldBinaryTree f x2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
    unfoldBinaryTree buildFunc 0
    where
        buildFunc x =
            if x >= n 
                then Nothing
                else Just (x + 1, x, x + 1) 