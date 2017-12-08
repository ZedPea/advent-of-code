import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- readLn
    print $ inverseCaptcha input

inverseCaptcha :: Integer -> Integer
inverseCaptcha n = let list = map (fromIntegral . digitToInt) $ show n
                   in  inverseCaptcha' list 0

-- this time we just take the whole list, and check that we haven't run out
-- of elements, if not, check if the next (length/2) elements == our element.
inverseCaptcha' :: [Integer] -> Int -> Integer
inverseCaptcha' xs n
    | n == length xs = 0
    | (xs !! n) == xs !! otherElement = (xs !! n) + inverseCaptcha' xs (n+1)
    | otherwise = inverseCaptcha' xs (n+1)
    where otherElement = (n + steps) `rem` len -- have to wrap around
          steps = len `div` 2
          len = length xs
