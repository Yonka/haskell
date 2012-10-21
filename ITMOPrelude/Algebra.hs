{-# LANGUAGE FlexibleInstances #-}

module ITMOPrelude.Algebra where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
       
class Monoid m where
	mempty :: m
	mappend :: m -> m -> m

newtype Product a = Product {getProduct :: a} deriving (Show)
newtype Sum a = Sum {getSum :: a}
	
instance Monoid (Product Nat) where
	mempty = Product natOne
	mappend (Product a) (Product b) = Product (a *. b)

instance Monoid (Sum Nat) where
	mempty = Sum natZero
	mappend (Sum a) (Sum b) = Sum (a +. b)

instance Monoid (Product Int) where
	mempty = Product intOne
	mappend (Product a) (Product b) = Product (a .*. b)

instance Monoid (Sum Int) where
	mempty = Sum intZero
	mappend (Sum a) (Sum b) = Sum (a .+. b)

instance Monoid (Product Rat) where
	mempty = Product ratOne
	mappend (Product a) (Product b) = Product (a %* b)

instance Monoid (Sum Rat) where
	mempty = Sum ratZero
	mappend (Sum a) (Sum b) = Sum (a %+ b)

instance Monoid a => Monoid (Maybe a) where
	mempty = Nothing
	mappend Nothing a = a
	mappend a Nothing = a
	mappend (Just a) (Just b) = Just (mappend a b)

newtype First a = First {getFirst :: Maybe a}

instance Monoid (First a) where
	mempty = First Nothing
	mappend (First Nothing) a = a
	mappend (First (Just a)) _ = First (Just a)

newtype AndBool = AndBool {getAnd :: Bool}
newtype OrBool = OrBool {getOr :: Bool}


instance Monoid(AndBool) where
	mempty = AndBool True
	mappend (AndBool True) (AndBool True) = AndBool True
	mappend _ _ = AndBool False

instance Monoid(OrBool) where
	mempty = OrBool False
	mappend (OrBool False) (OrBool False) = OrBool False
	mappend _ _ = OrBool False

instance Monoid(Tri) where
	mempty = EQ
	mappend LE _ = LE
	mappend GT _ = GT
	mappend EQ a = a

class Monoid a=> Group a where
	inverse :: a -> a

instance Group (Sum Int) where
	inverse (Sum a) = Sum (intNeg a)	

instance Group (Sum Rat) where
	inverse (Sum a) = Sum (ratNeg a)

instance Group (Product Rat) where
	inverse (Product a) = Product (ratInv a)
	