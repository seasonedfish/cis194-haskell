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
    show (P coefficients) = accumulateTerms coefficients 0 where
        accumulateTerms :: [a] -> Int -> String
        accumulateTerms [] _ = ""
        accumulateTerms (0:others) degree = accumulateTerms others (degree + 1)
        accumulateTerms (1:others) degree
                | degree == 0 = accumulateTerms others (degree + 1) ++ " + 1"
                | otherwise = accumulateTerms others (degree + 1) ++ " + x^" ++ show degree
        accumulateTerms [single] degree = show single ++ "x^" ++ show degree
        accumulateTerms (first:others) degree = accumulateTerms others (degree + 1) ++ " + " ++ show first ++ "x^" ++ show degree

                
-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

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

