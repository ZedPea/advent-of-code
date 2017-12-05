main :: IO ()
main = print =<< jmp 0 . map read . words <$> getContents

jmp :: Int -> [Int] -> Int
jmp _ [] = 0
jmp offset list@(x:xs)
    | offset < 0 || offset >= length list = 0
    | otherwise = 1 + jmp (offset + list !! offset) newList
    where newList
            | offset == 0 = x + 1 : xs
            | otherwise = let (start, val : end) = splitAt offset list
                          in   start ++ [val + 1] ++ end
