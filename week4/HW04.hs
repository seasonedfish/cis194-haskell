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

polyToList :: Poly a -> [a]
polyToList (P l) = l

instance forall a. (Num a, Eq a, Show a) => Show (Poly a) where
    show = flip accumulateTerms 0 . dropTrailingZeros . polyToList where
        accumulateTerms :: [a] -> Int -> String
        -- Special case: if a polynomial consisting of all zeroes is passed to the outer function,
        -- the inner function will be passed an empty list.
        accumulateTerms [] _ = "0"
        -- Base case.
        accumulateTerms [coefficient] degree = showTerm coefficient degree
        -- Recursive case.
        accumulateTerms (coefficient:coefficients) degree =
            accumulateTerms coefficients (degree + 1) ++ case coefficient of
                0 -> ""
                _ -> " + " ++ showTerm coefficient degree
        showTerm :: a -> Int -> String
        showTerm coefficient 0 = show coefficient
        showTerm 1 1 = "x"
        showTerm 1 degree = "x^" ++ show degree
        showTerm (-1) 1 = "-x"
        showTerm (-1) degree = "-x^" ++ show degree
        showTerm coefficient 1 = show coefficient ++ "x"
        showTerm coefficient degree = show coefficient ++ "x^" ++ show degree

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P []) = P []
plus (P [y]) (P []) = P [y]
plus (P []) (P l@(_:_)) = P l
plus (P l@(_:_:_)) (P []) = P l
plus (P (y:ys)) (P (z:zs)) = P ((y + z) : polyToList (plus (P ys) (P zs)))

-- Exercise 5 -----------------------------------------

times :: forall a. Num a => Poly a -> Poly a -> Poly a
times b c = sum (foil b c 0) where
    -- foil takes two polynomials and the starting degree of the first polynomial.
    -- It returns a list of polynomials that when summed, results in the product.
    -- foil (P [1, 1, 1]) (P [2, 2]) 0 == [P [2, 2], P [0, 2, 2], P [0, 0, 2, 2]]
    foil :: Poly a -> Poly a -> Int -> [Poly a]
    foil (P []) _ _ = []
    foil (P (y:ys)) (P m) leftDegree =
        shiftDegree (P [y * z | z <- m]) leftDegree : foil (P ys) (P m) (leftDegree + 1)
    -- shiftDegree increases the degree of each term in the given polynomial by the given int.
    -- shiftDegree 2 (P [1, 2, 3]) == P [0, 0, 1, 2, 3]
    shiftDegree :: Poly a -> Int -> Poly a
    shiftDegree (P l) degree = P (replicate degree 0 ++ l)



-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger y = P [fromIntegral y]
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
