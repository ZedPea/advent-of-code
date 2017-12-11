import Data.List.Split (splitOn)
import Control.Arrow (first)
import Data.Tuple.Curry (uncurryN)
import Data.Tuple.Select (sel3)

main :: IO ()
main = print =<< product . take 2 . sel3 . foldl (uncurryN hashList) (0, 0, [0..255]) . map read . splitOn "," <$> getLine

hashList :: Int -> Int -> [Int] -> Int -> (Int, Int, [Int])
hashList pos skipSize xs len = (newPos, skipSize+1, final)
    where flat = uncurry (flip (++)) $ splitAt pos xs
          rotated = uncurry (++) . first reverse $ splitAt len flat
          final = uncurry (flip (++)) $ splitAt (length rotated - pos) rotated
          newPos = (pos+len+skipSize) `mod` length xs
