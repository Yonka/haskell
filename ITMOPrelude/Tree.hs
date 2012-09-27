module ITMOPrelude.Tree where
 
import Prelude ((++), Show, Read, error) 

data Tree a = Node a (Tree a) (Tree a) | Nil deriving Show

empty = Nil

insert :: Tree a -> a -> Tree a
insert Nil a = Node a Nil Nil
insert t a = Node a t Nil

insertLeft :: Tree a -> a -> Tree a
insertLeft Nil a = Node a Nil Nil
insertLeft (Node t l r) a = Node t (insertLeft l a) r

insertRight :: Tree a -> a -> Tree a
insertRight Nil a = Node a Nil Nil
insertRight (Node t l r) a = Node t l (insertRight r a)

rotateRight :: Tree a -> Tree a
rotateRight (Node a Nil ar) = Node a Nil ar
rotateRight (Node a (Node b bl br) ar) = Node b bl (Node a br ar)

rotateLeft :: Tree a -> Tree a
rotateLeft (Node a al Nil) = Node a al Nil
rotateLeft (Node a al (Node b bl br)) = Node b (Node a al bl) br

map :: (a -> b) -> Tree a -> Tree b
map f Nil = Nil
map f (Node a l r) = Node (f a) (map f l) (map f r)
              
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Nil = z
foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l

t = Node "a" (Node "lvja" (Node "mv " Nil (Node ",c" Nil Nil)) (Node "nfh" Nil Nil)) (Node "dsf" Nil (Node "lvjv" (Node "f " Nil Nil) Nil)) 
a = " "
b = "sdf"
c = a ++ b

--            a
--         /     \
--     lvja        dsf
--   /      \         \
-- mv       nfh       lvjv
--   \               /
--    ,c             f
