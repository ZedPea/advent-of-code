import Data.List (nub)

main :: IO ()
main = do
    input <- map words . lines <$> getContents
    print . length $ filter (\xs -> length xs == length (nub xs)) input
