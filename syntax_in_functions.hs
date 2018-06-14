module SyntaxInFunctions where

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


tell :: (Show a) => [a] -> String
tell []       = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x
tell (x : y : []) =
  "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) =
  "This list is long. The first two elements are: "
    ++ show x
    ++ " and "
    ++ show y


capital :: String -> String
capital ""           = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5
  = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0
  = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0
  = "You're fat! Lose some weight, fatty!"
  | otherwise
  = "You're a whale, congratulations!"

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
  where bmi weight height = weight / height ^ 2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [ a | a <- xs, a <= x ]
      biggerSorted  = quicksort [ a | a <- xs, a > x ]
  in  smallerSorted ++ [x] ++ biggerSorted
