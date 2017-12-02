main :: IO ()
main = print =<< checksum . map (map read . words) . lines <$> getContents

checksum :: [[Int]] -> Int
checksum = sum . map checksum'

checksum' :: [Int] -> Int
checksum' [] = error "No evenly divisible values in list!"
checksum' (x:xs)
    | (Just result) <- evenDivisor x xs = result
    | otherwise = checksum' xs

evenDivisor :: Int -> [Int] -> Maybe Int
evenDivisor _ [] = Nothing
evenDivisor x (y:ys)
    | x `rem` y == 0 = Just (x `div` y)
    | y `rem` x == 0 = Just (y `div` x)
    | otherwise = evenDivisor x ys
