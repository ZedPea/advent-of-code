import Data.List (permutations)

main :: IO ()
main = print =<< length . filter isValid . map words . lines <$> getContents

isValid :: [String] -> Bool
isValid [] = True
isValid (x:xs)
    | any (\y -> x `elem` y) (map permutations xs) = False
    | otherwise = isValid xs
