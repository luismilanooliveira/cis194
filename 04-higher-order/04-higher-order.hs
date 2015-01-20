fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . takeWhile (>1) . iterate collatz
  where collatz n = if even n
                       then n `div` 2
                       else 3*n +1

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree t -> Integer
height Leaf = 0
height (Node n l _ r) = n


-- foldTree :: [a] -> Tree a
foldTree f = f insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf m Leaf) = Node (h+1) (insert x Leaf) m Leaf
insert x (Node h Leaf m r) = Node h (insert x Leaf) m r
insert x (Node h l m Leaf) = Node h l m (insert x Leaf)
insert x (Node h l@(Node lHeight _ _ _) m r@(Node rHeight _ _ _))
  | lHeight > rHeight = Node h l m (insert x r)
  | lHeight < rHeight = Node h (insert x l) m r
  | otherwise         = Node (h'+1) l' m r
    where l' = insert x l
          h' = height l'


xor :: [Bool] -> Bool
xor = foldr xor' False . filter (== True)
  where xor' = \q p -> if p == False
                          then q || False
                          else q && False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

myFoldl f acc xs = foldr f' id xs acc
  where f' x g a = g (f a x)

sundaram :: Integer -> [Integer]
sundaram n = map ((+1) . (*2)) . filter (`notElem` numbers) $ [1..n]
  where numbers = [i + j + 2*i*j | i <- [1..n], j <- [1..n]]
