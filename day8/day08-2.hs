import Data.Map.Lazy (Map, empty, insert, adjust, elems, (!))
import Data.Char (isAlpha)
import Data.List (isPrefixOf, tails, stripPrefix, isInfixOf)
import Data.Maybe (fromJust)

main :: IO ()
main = print =<< getMaxRegisterVal . lines <$> getContents

getMaxRegisterVal :: [String] -> Int
getMaxRegisterVal input = processInstructions input $ mkRegisters input

mkRegisters :: [String] -> Map String Int
mkRegisters = foldl insertRegister empty

insertRegister :: Map String Int -> String -> Map String Int
insertRegister mapping xs = insert second 0 $ insert first 0 mapping
    where (first, second) = parseRegisters xs

processInstructions :: [String] -> Map String Int -> Int
processInstructions input mapping
    = fst $ foldl processInstruction (0, mapping) input

processInstruction :: (Int, Map String Int) -> String -> (Int, Map String Int)
processInstruction (maxVal, mapping) instruction
    | secondVal `operator` val = (newMaxVal, newMapping)
    | otherwise = (maxVal, mapping)
    where (first, second) = parseRegisters instruction
          secondVal = mapping ! second
          operator = parseOperator instruction
          val = read . reverse . takeWhile isNumVal $ reverse instruction
          incrementVal = parseIncrementVal instruction
          newMapping = adjust (+incrementVal) first mapping
          newMaxVal
            | newMax > maxVal = newMax
            | otherwise = maxVal
            where newMax = maxMapVal mapping

parseRegisters :: String -> (String, String)
parseRegisters xs = (first, second)
    where first = takeWhile isAlpha xs
          second = takeWhile isAlpha stripped
          stripped = fromJust $ stripPrefix "if " targetStr
          targetStr = head . filter ("if " `isPrefixOf`) $ tails xs

parseOperator :: (Ord a) => String -> (a -> a -> Bool)
parseOperator xs
    | ">=" `isInfixOf` xs = (>=)
    | "<=" `isInfixOf` xs = (<=)
    | "==" `isInfixOf` xs = (==)
    | "!=" `isInfixOf` xs = (/=)
    | ">"  `isInfixOf` xs = (>)
    | "<"  `isInfixOf` xs = (<)
    | otherwise = error "Expected operator not found!"

parseIncrementVal :: String -> Int
parseIncrementVal xs 
    | "inc" `isInfixOf` xs = val
    | otherwise = -val
    where val = read . takeWhile isNumVal $ dropWhile (not . isNumVal) xs

maxMapVal :: Map String Int -> Int
maxMapVal = maximum . elems

isNumVal :: Char -> Bool
isNumVal = (`elem` numVals)
    where numVals = '-' : ['0'..'9']
