import Data.List (nub, sort)
import Control.Applicative (liftA2)

main :: IO ()
main = print =<< length . filter (liftA2 (==) length (length . nub . map sort)) . map words . lines <$> getContents
