{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons _ a) = natOne +. length a

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ a = a
(Cons a b) ++ c = Cons a (b ++ c) 

-- Список без первого элемента
tail :: List a -> List a
tail (Cons a b) = b

-- Список без последнего элемента
init :: List a -> List a
init (Cons a Nil) = Nil
init (Cons a b) = Cons a (init b)

-- Первый элемент
head :: List a -> a
head (Cons a _) = a

-- Последний элемент
last :: List a -> a
last (Cons a Nil) = a
last (Cons a b) = last b

-- n первых элементов списка
take :: Nat -> List a -> List a
take _ Nil = Nil
take Zero _ = Nil
take (Succ a) (Cons b c) = Cons b (take a c) 

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop _ Nil = Nil
drop Zero a = a
drop (Succ a) (Cons b c) = drop a c

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a b) = case p a of
	True -> Cons a (filter p b)
	False -> filter p b

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a b) = case p a of 
	Nothing -> gfilter p b
	Just c -> Cons c (gfilter p b)

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a b) = case p a of
	False  -> Nil
	True -> Cons a (takeWhile p b)

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a b) = case p a of
	True -> dropWhile p b
	False -> Cons a b

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons a b) = case p a of
	True -> Pair (Cons a (first)) second where
		Pair first second = span p b 
	False -> Pair Nil (Cons a b)

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a 

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
Cons a b !! Zero = a
Cons a b !! Succ n = b !! n 

-- Список задом наперёд
reverse :: List a -> List a
reverse a = rev' Nil a where
	rev' a Nil = a
	rev' a (Cons b c) = rev' (Cons b a) c  

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = t ++ (map (Cons x) t) where t = subsequences xs

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil
permutations a = extr (length a) a

extr :: Nat -> List a -> List (List a)
extr Zero a = Nil
extr (Succ a) (Cons b c) = (map (Cons b) $ permutations c) ++ (extr a $ reverse (Cons b $ reverse c) )
	
-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f a Nil = a
foldl f z (Cons a b) = foldl f (f z a) b

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f a Nil = Cons a Nil
scanl f z (Cons a b) = Cons t (scanl f t b) where t = f z a

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f b Nil = b
foldr f z (Cons a b) = f a (foldr f z b)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f b Nil = Cons b Nil
scanr f z (Cons a b) = Cons t p where 
	t = f a (head (p))
	p = scanr f z b

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil 

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons a b) = f a ++ concatMap f b

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)            
zip = zipWith Pair


-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons a b) (Cons c d) = Cons (f a c) (zipWith f b d)






-- ////////////////
a = Cons 2 (Cons 3 (Cons 5(Cons 1 Nil)))
b = Cons 1 $Cons 2 Nil