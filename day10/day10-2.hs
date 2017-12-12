import Data.List.Split (splitOn, chunksOf)
import Control.Arrow (first)
import Data.Tuple.Curry (uncurryN)
import Data.Tuple.Select (sel3)
import Data.Char (ord, chr, toLower)
import Data.Hex (hex)
import Data.Bits (xor)

main :: IO ()
main = do
    lengths <- map ord <$> getLine
    let adjusted = lengths ++ [17, 31, 73, 47, 23]
        knotted = sel3 $ knot 64 0 0 [0..255] adjusted
    putStrLn $ hash knotted

hash :: [Int] -> String
hash = map toLower . hex . map chr . map (foldl1 xor) . chunksOf 16

knot :: Int -> Int -> Int -> [Int] -> [Int] -> (Int, Int, [Int])
knot 0 pos skipSize xs _ = (pos, skipSize, xs)
knot iterations pos skipSize xs lengths = 
    let (newPos, newSkipSize, newXs) = 
            foldl (uncurryN knotList) (pos, skipSize, xs) lengths
    in  knot (iterations-1) newPos newSkipSize newXs lengths

knotList :: Int -> Int -> [Int] -> Int -> (Int, Int, [Int])
knotList pos skipSize xs len = (newPos, skipSize+1, final)
    where flat = uncurry (flip (++)) $ splitAt pos xs
          rotated = uncurry (++) . first reverse $ splitAt len flat
          final = uncurry (flip (++)) $ splitAt (length rotated - pos) rotated
          newPos = (pos+len+skipSize) `mod` length xs
