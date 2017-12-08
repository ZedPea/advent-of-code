main :: IO ()
main = print =<< checksum . map (map read . words) . lines <$> getContents

checksum :: [[Int]] -> Int
checksum = sum . map checksum'

checksum' :: [Int] -> Int
checksum' xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `rem` y == 0]
