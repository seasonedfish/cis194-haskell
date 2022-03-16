sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums
    where go :: Int -> [Int] -> Int
          go acc [] = acc
          go acc (x:xs)
            | acc >= 20 = acc
            | otherwise = go (acc + x) xs


myHead :: [a] -> a
myHead (a:_) = a


