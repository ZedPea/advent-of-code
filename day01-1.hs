import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- readLn
    print $ inverseCaptcha input

-- have to check if last element == first element, so just pop last on end
-- of list, we terminate when there aren't two elements to compare
inverseCaptcha :: Integer -> Integer
inverseCaptcha n = let list = map (fromIntegral . digitToInt) $ show n
                   in  inverseCaptcha' (list ++ [head list])

inverseCaptcha' :: [Integer] -> Integer
inverseCaptcha' (x:y:xs)
    | x == y = x + inverseCaptcha' (y:xs)
    | otherwise = inverseCaptcha' (y:xs)
inverseCaptcha' _ = 0
