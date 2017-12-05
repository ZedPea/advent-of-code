import qualified Data.Vector.Mutable as M (length, read, modify, new, write)

main :: IO ()
main = do
    input <- map read . words <$> getContents
    print =<< jmp input

jmp :: [Int] -> IO Integer
jmp xs = do
    vec <- M.new $ length xs
    fromList vec xs 0
    jmp' 0 vec

fromList vec [] _ = return ()
fromList vec (x:xs) n = do
    M.write vec n x
    fromList vec xs (n+1)

jmp' offset vec
    | offset < 0 || offset >= M.length vec = return 0
    | otherwise = do
        jmpVal <- M.read vec offset
        M.modify vec (\x -> x + (if jmpVal >= 3 then (-1) else 1)) offset
        nextVal <- jmp' (offset + jmpVal) vec
        return $ 1 + nextVal
