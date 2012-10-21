module ITMOPrelude.Primitive where
import Prelude (Show,Read, error)

undefined = undefined

data Unit = Unit deriving (Show,Read)
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)
data Either a b = Left a | Right b deriving (Show,Read)
data Maybe a = Nothing | Just a deriving (Show,Read)
data Bool = False | True deriving (Show,Read)


data Tri = LE | EQ | GT deriving (Show,Read)
data Nat = Zero | Succ Nat deriving (Show,Read)

not :: Bool -> Bool
not True = False
not False = True

natZero = Zero
natOne = Succ Zero


natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero _ = LE
natCmp _ Zero = GT
natCmp (Succ a) (Succ b) = natCmp a b


natEq :: Nat -> Nat -> Bool
natEq Zero Zero = True
natEq Zero (Succ _) = False
natEq (Succ _) Zero = False
natEq (Succ n) (Succ m) = natEq n m


natLe :: Nat -> Nat -> Bool
natLe Zero Zero = False
natLe Zero (Succ _) = True
natLe (Succ _) Zero = False
natLe (Succ n) (Succ m) = natLe n m

natGt :: Nat -> Nat -> Bool
natGt n m = natLe m n

infixl 6 +.
(+.) :: Nat -> Nat -> Nat
Zero +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
(*.) :: Nat -> Nat -> Nat
Zero *. m = Zero
(Succ n) *. m = m +. (n *. m)

natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = case n `natLe` m of
	False -> Pair (Succ (fst (natDivMod (n -. m) m))) (snd (natDivMod (n -. m) m))
	True -> Pair Zero n 

natDiv n = fst . natDivMod n
natMod n = snd . natDivMod n

gcd :: Nat -> Nat -> Nat
gcd Zero m = m
gcd n m = gcd (m `natMod` n) n

-------------------------------------------
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero = Pos Zero
intOne = Pos (Succ Zero)
intNegOne = Neg Zero 

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = intZero
intNeg (Pos (Succ a)) = Neg a
intNeg (Neg a) = Pos (Succ a)

intCmp :: Int -> Int -> Tri
intCmp (Pos a) (Pos b) = natCmp a b
intCmp (Neg a) (Pos b) = LE
intCmp (Pos a) (Neg b) = GT
intCmp (Neg a) (Neg b) = natCmp b a

intEq :: Int -> Int -> Bool
intEq a b = case intCmp a b of
	EQ -> True
	_ -> False

intLe :: Int -> Int -> Bool
intLe a b = case intCmp a b of
	LE -> True
	_ -> False

intGt :: Int -> Int -> Bool
intGt a b = intLe b a

infixl 6 .+., .-.

(.+.) :: Int -> Int -> Int
(Pos a) .+. (Pos b) = Pos (a +. b)
(Neg a) .+. (Neg b) = Neg ( Succ(a +. b) )
(Pos a) .+. (Neg b) = case natGt a b of
	False -> Neg (b -. a)
	True -> Pos (a -. b -. natOne)
a .+. b = b .+. a

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos a) .*. (Pos b) = Pos (a *. b)
(Neg a) .*. (Pos b)  = intNeg (intNeg (Neg a) .*. (Pos b))
a .*. b = b .*. a

infixl 7 .*
(.*) :: Int -> Nat -> Int
x .* d = x .*. (Pos d)
-------------------------------------------
data Rat = Rat Int Nat deriving (Show,Read)

normalizeRat :: Rat -> Rat
normalizeRat (Rat (Pos b) c) = Rat (Pos (natDiv b (gcd b c))) (natDiv c (gcd b c))
normalizeRat (Rat (Neg b) c) = Rat (Neg (natDiv b (gcd b c))) (natDiv c (gcd b c))

ratZero = Rat intZero natOne
ratOne = Rat intOne natOne

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = normalizeRat (Rat (intNeg x) y)

ratInv :: Rat -> Rat
ratInv (Rat (Pos b) c) = Rat (Pos c) b 
ratInv (Rat (Neg b) c) = Rat (Neg c) b 

ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a1 b1) (Rat a2 b2) = intCmp (a1 .* b2) (a2 .* b1) 

ratEq :: Rat -> Rat -> Bool
ratEq a b = case ratCmp a b of
	EQ -> True
	_ -> False

ratLe :: Rat -> Rat -> Bool
ratLe a b = case ratCmp a b of
	LE -> True
	_ -> False

ratGt :: Rat -> Rat -> Bool
ratGt a b = ratLe b a

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a1 b1) %+ (Rat a2 b2) = normalizeRat (Rat (a1 .* b2 .+. a2 .* b1) (b1 *. b2))
 
(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a1 b1) %* (Rat a2 b2) = normalizeRat (Rat (a1 .*. a2) (b1 *. b2))

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

n7 = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
n49 = n7 *. n7
n5 = (Succ (Succ (Succ (Succ (Succ Zero)))))
ip5 = Pos n5
ip7 = Pos n7
in5 = Neg n5
in7 = Neg n7
rp55 = Rat ip5 n5
rp57 = Rat ip5 n7
rn55 = Rat in5 n5
rn57 = Rat in5 n7
