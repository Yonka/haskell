{-# LANGUAGE FlexibleInstances #-}

module ITMOPrelude.Categories where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List
import qualified ITMOPrelude.Tree as T

class Functor f where
	fmap :: (a -> b) -> f a -> f b

instance Functor List where
	fmap = map

instance Functor Maybe where
	fmap f (Just x) = Just (f x)
	fmap f Nothing = Nothing

instance Functor T.Tree where
	fmap = T.map

instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x

class Monad m where
	return :: a -> m a
	(>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
	return x = Just x
	Nothing >>= f = Nothing
	Just x >>= f = f x

instance Monad List where
	return x = Cons x Nil
	Cons a b >>= f = f a ++ (b >>= f)

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State $ \s -> (s, a)
    act >>= f = State $ \s ->
		let (s', a) = runState act s
		in runState (f a) s'
