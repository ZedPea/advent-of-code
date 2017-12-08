import Data.List (nub)
import Control.Applicative (liftA2)

main :: IO ()
main = print =<< length . filter (liftA2 (==) length (length . nub)) . map words . lines <$> getContents
