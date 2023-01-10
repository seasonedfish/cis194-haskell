{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HW04 where

import Data.Function

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

dropTrailingZeros :: (Eq a, Num a) => [a] -> [a]
dropTrailingZeros poly = reverse poly & dropWhile (==0) & reverse


instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P coefficients1) (P coefficients2) = dropTrailingZeros coefficients1 == dropTrailingZeros coefficients2
 
-- Exercise 3 -----------------------------------------

instance forall a. (Num a, Eq a, Show a) => Show (Poly a) where
    show (P coefficients) = accumulateTerms (dropTrailingZeros coefficients) 0 where
        accumulateTerms :: [a] -> Int -> String
        -- Special case: if a polynomial consisting of all zeroes is passed to the outer function,
        -- the inner function will be passed an empty list.
        accumulateTerms [] _ = "0"
        -- Base case.
        accumulateTerms [single] degree = show single ++ showX degree
        -- Recursive cases.
        accumulateTerms (first:others) degree = case first of
            0 -> accumulateTerms others (degree + 1)
            1 | degree == 0 -> accumulateTerms others (degree + 1) ++ " + 1"
              | otherwise -> accumulateTerms others (degree + 1) ++ " + " ++ showX degree
            -1 | degree == 0 -> accumulateTerms others (degree + 1) ++ " + -1"
               | otherwise -> accumulateTerms others (degree + 1) ++ " + -" ++ showX degree
            _ -> accumulateTerms others (degree + 1) ++ " + " ++ show first ++ showX degree
        showX :: Int -> String
        showX degree
            | degree == 0 = ""
            | degree == 1 = "x"
            | otherwise = "x^" ++ show degree

                
-- Exercise 4 -----------------------------------------

polyToList :: Poly a -> [a]
polyToList (P y) = y

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P []) = P []
plus (P [y]) (P []) = P [y]
plus (P (y:ys)) (P (z:zs)) = P ((y + z) : polyToList (plus (P ys) (P zs)))
plus (P []) (P (z:zs)) = P (z : polyToList (plus (P []) (P zs)))
plus (P (y1:y2:ys)) (P []) = P ([y1, y2] ++ polyToList (plus (P ys) (P [])))


-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

