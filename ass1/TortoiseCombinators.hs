module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen Stop i2 = i2
andThen i1 Stop = i1
andThen (Move x xs) i2 = Move x $ andThen xs $ i2
andThen (Turn x xs) i2 = Turn x $ andThen xs $ i2
andThen (SetStyle x xs) i2 = SetStyle x $ andThen xs $ i2
andThen (SetColour x xs) i2 = SetColour x $ andThen xs $ i2
andThen (PenDown xs) i2 = PenDown $ andThen xs $ i2
andThen (PenUp xs) i2 = PenUp $ andThen xs $ i2


loop :: Int -> Instructions -> Instructions
loop n i 
 | n <= 0      = Stop
 | otherwise  =  andThen i $ loop (n - 1) i




invisibly :: Instructions -> Instructions
invisibly i = invisibly' i True
       where 
              invisibly':: Instructions -> Bool -> Instructions
              invisibly' Stop True = PenDown Stop
              invisibly' Stop False = PenUp Stop
              invisibly' (PenDown xs) boo =  invisibly' xs True
              invisibly' (PenUp xs) boo =  invisibly' xs False
              invisibly' (Move x xs) boo = PenUp $ Move x $ invisibly' xs boo
              invisibly' (Turn x xs) boo = Turn x $ invisibly' xs boo
              invisibly' (SetStyle x xs) boo = SetStyle x $ invisibly' xs boo
              invisibly' (SetColour x xs) boo = SetColour x $ invisibly' xs boo


retrace :: Instructions -> Instructions
retrace i = retrace' i Stop (Solid 1) white True
       where 
              retrace' :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
              retrace' Stop i line colour True      = SetColour colour $ SetStyle line $ PenDown $ i 
              retrace' Stop i line colour False      = SetColour colour $ SetStyle line $ PenUp $ i 

              retrace' (Move x xs) i line colour boo = retrace' xs (Move ((-) 0 x) i) line colour boo
              retrace' (Turn x xs) i line colour boo = retrace' xs (Turn ((-) 0 x) i) line colour boo
              

              retrace' (SetStyle x xs) i line colour boo  = retrace' xs (SetStyle line i) x colour boo 

              retrace' (SetColour x xs) i line colour boo  = retrace' xs (SetColour colour i) line x boo 

              retrace' (PenDown xs) i line colour True  = retrace' xs (PenDown i) line colour True
              retrace' (PenUp xs) i line colour True  = retrace' xs (PenDown i) line colour False

              retrace' (PenDown xs) i line colour False  = retrace' xs (PenUp i) line colour True
              retrace' (PenUp xs) i line colour False  = retrace' xs (PenUp i) line colour False
   


overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay (x:xs) =andThen (andThen x (invisibly $ retrace x)) (overlay xs)

