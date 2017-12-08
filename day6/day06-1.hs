import Data.Set (Set, empty, member, insert)

main :: IO ()
main = do
    input <- map read . words <$> getLine
    print $ numCycles empty input

numCycles :: Set [Int] -> [Int] -> Int
numCycles knownStates bank
    | bank `member` knownStates = 0
    | otherwise = 1 + numCycles (insert bank knownStates) newBank
    where newBank = reorder bank

reorder :: [Int] -> [Int]
reorder bank = let (largestIndex, largestVal) = getMaxValIndex bank 0 (0, 0)
                   (start, _:end) = splitAt largestIndex bank
                   zeroedBank = start ++ [0] ++ end
               in  addBlocks zeroedBank (largestIndex + 1) largestVal

addBlocks :: [Int] -> Int -> Int -> [Int]
addBlocks newBank _ 0 = newBank
addBlocks bank index blocksRemaining
    | index == length bank = addBlocks bank 0 blocksRemaining
    | otherwise = addBlocks newBank (index+1) (blocksRemaining-1)
    where (start, val:end) = splitAt index bank
          newBank = start ++ [val+1] ++ end

getMaxValIndex :: [Int] -> Int -> (Int, Int) -> (Int, Int)
getMaxValIndex bank index old@(_, largestVal)
    | index >= length bank = old
    | val > largestVal = getMaxValIndex bank (index+1) (index, val)
    | otherwise = getMaxValIndex bank (index+1) old
    where val = bank !! index
