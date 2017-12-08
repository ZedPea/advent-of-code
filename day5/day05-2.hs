import qualified Data.Vector.Mutable as M (length, read, modify, new, write)
import Data.Vector.Mutable (MVector)
import Control.Monad.Primitive (PrimState)
import Control.Monad (zipWithM_)

main :: IO ()
main = do
    input <- map read . words <$> getContents
    print =<< jmp input

jmp :: [Int] -> IO Integer
jmp xs = do
    vec <- M.new $ length xs
    fromList vec xs
    jmp' 0 vec

fromList :: MVector (PrimState IO) a -> [a] -> IO ()
fromList vec xs = zipWithM_ (M.write vec) [0 .. length xs - 1] xs

jmp' :: Int -> MVector (PrimState IO) Int -> IO Integer
jmp' offset vec
    | offset < 0 || offset >= M.length vec = return 0
    | otherwise = do
        jmpVal <- M.read vec offset
        M.modify vec (+ (if jmpVal >= 3 then (-1) else 1)) offset
        fmap (+1) (jmp' (offset + jmpVal) vec)
