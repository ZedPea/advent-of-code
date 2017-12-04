import Data.List (permutations)

main :: IO ()
main = do
    input <- map words . lines <$> getContents
    print . length $ filter isValid input

isValid :: [String] -> Bool
isValid [] = True
isValid (x:xs)
    | any (\y -> x `elem` y) (map permutations xs) = False
    | otherwise = isValid xs
