module Ex02 where
import Test.QuickCheck
import Data.List


-- implement the following functions, which meet some (but not all!) of the
-- properties of a correct sorting function

-- prop1 & 4, but not prop2 & 3 & 5
dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = xs


-- prop2 & 3 & 4, but not prop1 & 5
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = sort (nub xs)


-- prop1 & 2 & 3, but not prop4 & 5
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs = sort $ tmp xs
  where 
    tmp::[Int]->[Int]
    tmp [] = []
    tmp [x] = [0]
    tmp xs = [0] ++ tail (init xs) ++ [0]

-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = sort $ tmp xs
  where 
    tmp::[Int]->[Int]
    tmp [] = []
    tmp [x] = [x]
    tmp xs = fillwithZero (length xs - (length (nub xs))) (nub xs)

    fillwithZero:: Int -> [Int] -> [Int]
    fillwithZero 0 xs = xs
    fillwithZero x xs = fillwithZero (x-1) (xs ++ [0])

-- Properties of sorting function
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = length xs == length (sortFn xs)

sortProp2 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp2 sortFn xs = sortFn xs == sortFn (reverse xs)

sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

sortProp4 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp4 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs
  = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where
    insertSorted x [] = [x]
    insertSorted x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys

