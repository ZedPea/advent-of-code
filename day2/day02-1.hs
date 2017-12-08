import Control.Applicative (liftA2)

main :: IO ()
main = print =<< sum . map (liftA2 (-) maximum minimum) . map (map read . words) . lines <$> getContents
