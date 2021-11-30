{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 


data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a, b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b


match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do
  x <- readCharacter
  guard (x `elem` cs)
  pure x
match (Seq a b) = do 
    ra <- match a 
    rb <- match b  
    pure (ra, rb)
match (Choose a b) = 
        match a
    <|> match b
match (Star a) =
        (:) <$> match a <*> match (Star a)
    <|> pure []
match (Action f a) = fmap f (match a)


matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)




infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action tmp (Seq x xs)


--helper function
tmp :: (a, [a]) -> [a]
tmp (x, xs) = x:xs



plus :: RE a -> RE [a]
plus re = Action tmp (Seq re (Star re))

--helper function1
tmp1 :: () -> [a]
tmp1 _ = []

string :: String -> RE String
string [] = Action tmp1 Empty
string (x:xs) = cons (Char [x]) (string xs)


choose :: [RE a] -> RE a
choose [] = Fail
choose (x:xs) = Choose x (choose xs)

tmp2 :: () -> Maybe a
tmp2 _ = Nothing

tmp3 :: a -> Maybe a
tmp3 x = Just x

option :: RE a -> RE (Maybe a)
option re = Choose (Action tmp3 re) (Action tmp2 Empty)


rpt :: Int -> RE a -> RE [a]
rpt 0 re = Action tmp1 Empty
rpt n re = Action tmp (Seq re (rpt (n - 1) re)) 

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x, y) re
  | x == y = rpt y re
  | otherwise = Choose (rpt y re) (rptRange (x, y-1) re)
