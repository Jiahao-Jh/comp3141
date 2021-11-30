module Ex05 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise s =  mapM parseToken (words s)


newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
pop = (C pop')
  where 
    pop' :: [Int] -> Maybe ([Int], Int)
    pop' [] = Nothing
    pop' xs = Just (tail xs, head xs)

push :: Int -> Calc ()
push i = C push'
  where
    push' :: [Int] -> Maybe ([Int], ())
    push' xs = Just (i : xs , ())


instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a



evaluate :: [Token] -> Calc Int
evaluate [] = pop
evaluate ts = 
  case (head ts) of 
    Number x   -> do push x
                     evaluate $ tail ts
    Operator o -> do x <- pop
                     y <- pop
                     push (o y x)
                     evaluate $ tail ts


calculate :: String -> Maybe Int
calculate s = cal (tokenise s)
  where
    cal :: Maybe [Token] -> Maybe Int
    cal Nothing = Nothing
    cal (Just xs) = cal' (evaluate xs)

    cal' :: Calc Int -> Maybe Int
    cal' (C f) = fmap snd (f [])


-- calculate :: String -> Maybe Int
-- calculate s = cal' $ fmap $ evaluate . tokenise $ s
--   where 
--     cal' :: Maybe (Calc Int) -> Maybe Int
--     cal' Nothing = Nothing
--     cal' (Just pop) = qqq pop

--     qqq :: Calc Int -> Maybe Int
--     qqq (C f) = fmap snd (f [])

-- (tokenise s) :: 
-- evaluate :: [Token] -> Calc Int
-- calculate :: Maybe [Token] -> Maybe Int

-- :t fmap  evaluate . tokenise
-- fmap  evaluate . tokenise :: String -> Maybe (Calc Int)