module HighOrderFunctions where

{-| Multiplies 3 numbers.
 -}
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

{-| Takes a number and compares it to 100 -}
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

{-| Infix functions can also be partially applied by using sections. To section
 - an infix function, simply surround it with parentheses and only supply a
 - parameter on one side. That creates a function that takes one parameter and
 - then applies it to the side that's missing an operand.
 -}
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

{-| Checks if a character supplied to it is an uppercase letter. -}
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

{-| Takes a function and then applies it twice to something! -}
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

{-| It takes a function and two lists as parameters and then joins the two
 - lists by applying the function between corresponding elements.
 -}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

{-| Flip simply takes a function and returns a function that is like our
 - original function, only the first two arguments are flipped.
 -}
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

{-| Quicksort using filter -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted  = quicksort (filter (> x) xs)
  in  smallerSorted ++ [x] ++ biggerSorted

{-| Find the largest number under 100,000 that's divisible by 3829. -}
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where p x = x `mod` 3829 == 0

{-| Collatz sequences:
 - We take a natural number.
 - If that number is even, we divide it by two.
 - If it's odd, we multiply it by 3 and then add 1 to that.
 - We take the resulting number and apply the same thing to it, which produces a
 - new number and so on.
 -
 - In essence, we get a chain of numbers. It is thought that for all starting
 - numbers, the chains finish at the number 1.
 -
 - So if we take the starting number 13, we get this sequence:
 -   13, 40, 20, 10, 5, 16, 8, 4, 2, 1.
 -
 - 13*3 + 1 equals 40. 40 divided by 2 is 20, etc.
 - We see that the chain has 10 terms.
 -}
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
        | odd n  = n : chain (n * 3 + 1)

{-| We map the chain function to [1..100] to get a list of chains, which are
 - themselves represented as lists. Then, we filter them by a predicate that
 - just checks whether a list's length is longer than 15. Once we've done the
 - filtering, we see how many chains are left in the resulting list.
 - -}
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))


-- Only folds and horses

{-| Sum using fold left. -}
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

{-| Custom map using right fold. -}
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
