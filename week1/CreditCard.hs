-- Convert positive Integers to a list of digits
-- e.g. 1234 -> [1, 2, 3, 4]
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]


-- Same but reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = [n `mod` 10] ++ toDigitsRev (n `div` 10)


-- Double every other int in list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (firstPart ++ [secondToLast, last]) = doubleEveryOther firstPart ++ [2 * secondToLast, last] 
