module Ex03 where

import Test.QuickCheck
import Data.List(sort, nub)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)



--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r)
  | i < v     = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where
   searchTrees' 0 = return Leaf
   searchTrees' n = do
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

----------------------

mysteryProp :: Integer -> BinaryTree -> Int
mysteryProp i Leaf = 0
mysteryProp i (Branch a b c) 
  | i == a    = (mysteryProp i b) +  (mysteryProp i c) +1
  | otherwise = (mysteryProp i b) +  (mysteryProp i c)

prop_mysteryProp_1 integer =
  forAll searchTrees $ \tree ->
    mysteryProp integer (insert integer tree) > mysteryProp integer tree

prop_mysteryProp_2 integer =
  forAll searchTrees $ \tree ->
    mysteryProp integer (deleteAll integer tree) == 0

----------------------
mysterious :: BinaryTree -> [Integer]
mysterious t = sort $ mysterious' t []
  where 
    mysterious' :: BinaryTree -> [Integer] -> [Integer]
    mysterious' Leaf a = a
    mysterious' (Branch a b c) ls 
     | a `elem` ls = (mysterious' b ls) ++ (mysterious' c ls)
     | otherwise   = [a] ++ (mysterious' b ls) ++ (mysterious' c ls)


isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

prop_mysterious_1 integer = forAll searchTrees $ \tree ->
  mysteryProp integer tree == (numInt $ mysterious tree)
   where
     numInt = length . filter (== integer)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious
----------------------


-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf
astonishing xs = Branch (head b2) (astonishing b1) (astonishing $ tail b2)
  where
    (b1, b2) = splitInHalf xs

splitInHalf :: [Integer] -> ([Integer],[Integer])
splitInHalf xs =  splitAt ((length xs) `div` 2) (xs)

prop_astonishing_1
  = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2
  = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3
  = forAll sortedListsWithoutDuplicates $ \ integers ->
    mysterious (astonishing integers) == integers


isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r)
  = allTree (< v) l  &&
    allTree (>= v) r &&
    isBST l          &&
    isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True


isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = and [ abs (height l - height r) <= 1
                                , isBalanced l
                                , isBalanced r
                                ]
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)

