{-# LANGUAGE FlexibleContexts #-}
module Ex04 where
import Text.Read (readMaybe)
import System.IO
import Data.Char
import System.Environment
import Control.Monad.State
import System.Random
import Test.QuickCheck

onlyAlphabetic :: FilePath -> FilePath -> IO ()
onlyAlphabetic i o = do
  fileInput <- readFile i
  writeFile o $ onlyA fileInput
  where 
    onlyA :: String -> String
    onlyA xs = filter (\x -> isAlpha x) xs




fileProduct :: IO ()
fileProduct = do
  args <- getArgs
  fileInput <- readFile $ head args
  let 
    list = f $ words fileInput
    result = show $ product list 
    f :: [String] -> [Integer]
    f = map read
  writeFile (args !! 1) result



data Player m = Player { guess :: m Int
                       , wrong :: Answer -> m ()
                       }
data Answer = Lower | Higher


human :: Player IO
human = Player { guess = guess, wrong = wrong }
  where
    guess = do
      putStrLn "Enter a number (1-100):"
      x <- getLine
      case readMaybe x of
        Nothing -> guess
        Just i  -> pure i

    wrong Lower  = putStrLn "Lower!"
    wrong Higher = putStrLn "Higher!"

play :: IO ()
play = do
  x <- randomRIO (1,100)
  b <- guessingGame x 5 human
  putStrLn (if b then "You got it!" else "You ran out of guesses!")




midpoint :: Int -> Int -> Int
midpoint lo hi | lo <= hi  = lo + div (hi - lo) 2
               | otherwise = midpoint hi lo

ai :: Player (State (Int,Int))
ai = Player { guess = guess, wrong = wrong }
  where
    guess = do
      (a,b) <- get
      pure $ midpoint a b

    wrong Lower = 
      modify $ \(a,b) -> (a, midpoint a b)

    wrong Higher =
      modify $ \(a,b) -> ((midpoint a b) + 1, b)
                                                         
guessingGame :: (Monad m) => Int -> Int -> Player m -> m Bool
guessingGame x n p = go n
  where
   go 0 = pure False
   go n = do
     x' <- guess p
     case compare x x' of
       LT -> wrong p Lower  >> go (n-1)
       GT -> wrong p Higher >> go (n-1)
       EQ -> pure True

prop_basic (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x n ai) (1,n)

prop_optimality (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x (bound n) ai) (1,n)
  where bound n = ceiling (logBase 2 (fromIntegral n)) + 1


