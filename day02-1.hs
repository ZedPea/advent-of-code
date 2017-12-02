import Control.Applicative (liftA2)

main :: IO ()
main = print =<< checksum . map (map read . words) . lines <$> getContents

checksum :: [[Int]] -> Int
checksum = sum . map (liftA2 (-) maximum minimum)
