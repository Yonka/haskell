{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \s -> case (stdIn s) of
	Cons a b -> (RealWorld b (stdOut s) (exitCode s), a) 
	Nil -> (RealWorld (Nil) (stdOut s) (exitCode s), Zero)

putNat :: Nat -> IO ()
putNat x = State $ \s -> (RealWorld (stdIn s) (Cons x (stdOut s) ) (exitCode s), ())

setExitCode :: Nat -> IO ()
setExitCode x = State $ \s -> (RealWorld (stdIn s) (stdOut s) x, ()) 
