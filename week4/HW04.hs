{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

dropTrailingZeros :: (Num a, Eq a) => Poly a -> Poly a
dropTrailingZeros (P l) = (P . reverse . dropWhile (== 0) . reverse) l

instance (Num a, Eq a) => Eq (Poly a) where
    (==) p q = dropTrailingZeros p `exactlyEquals` dropTrailingZeros q where
        exactlyEquals :: Poly a -> Poly a -> Bool
        exactlyEquals (P l) (P m) = l == m

-- Exercise 3 -----------------------------------------

instance forall a. (Num a, Eq a, Show a) => Show (Poly a) where
    show = flip accumulateString 0 . dropTrailingZeros where
        accumulateString :: Poly a -> Int -> String
        -- Special case: if a polynomial consisting of all zeroes is passed to the outer function,
        -- the inner function will be passed an empty list.
        accumulateString (P []) _ = "0"
        -- Base case.
        accumulateString (P [y]) degree = showTerm y degree
        -- Recursive case.
        accumulateString (P (y:ys)) degree =
            accumulateString (P ys) (degree + 1) ++ case y of
                0 -> ""
                _ -> " + " ++ showTerm y degree
        showTerm :: a -> Int -> String
        showTerm coefficient 0 = show coefficient
        showTerm 1 1 = "x"
        showTerm 1 degree = "x^" ++ show degree
        showTerm (-1) 1 = "-x"
        showTerm (-1) degree = "-x^" ++ show degree
        showTerm coefficient 1 = show coefficient ++ "x"
        showTerm coefficient degree = show coefficient ++ "x^" ++ show degree

-- Exercise 4 -----------------------------------------

consP :: a -> Poly a -> Poly a
consP y (P l) = P (y:l)

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P []) = P []
plus (P [y]) (P []) = P [y]
plus (P []) (P l@(_:_)) = P l
plus (P l@(_:_:_)) (P []) = P l
plus (P (y:ys)) (P (z:zs)) = (y + z) `consP` plus (P ys) (P zs)

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
    negate p = P [-1] * p
    fromInteger y = P [fromIntegral y]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: forall a. Num a => Poly a -> a -> a
applyP (P l) val =
    sum $ zipWith (\y z -> y * val^z) l [0::Integer ..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    -- https://stackoverflow.com/a/3911161
    nderiv n = foldr (.) id (replicate n deriv)

-- Exercise 9 -----------------------------------------

instance forall a. Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ys)) = P (zipWith (*) ys oneToInf) where
        oneToInf = map fromInteger [1..]
