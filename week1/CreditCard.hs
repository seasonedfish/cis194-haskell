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
-- starting from the end
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther list = doubleEveryOther (take (length list - 2) list) ++ doubleFirst (drop (length list - 2) list)


-- If the list has one element, don't double.
-- Otherwise, double the first element.
doubleFirst :: [Integer] -> [Integer]
doubleFirst (first:[]) = [first]
doubleFirst (first:otherElements) = first * 2 : otherElements
