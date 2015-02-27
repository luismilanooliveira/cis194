data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

treeFold :: (a -> b -> a -> a) -> a -> Tree b -> a
treeFold _ e Empty        = e
treeFold f e (Node l x r) = f (treeFold f e l) x (treeFold f e r)

treeSize = treeFold (\ l _ r -> 1 + l + r) 0
