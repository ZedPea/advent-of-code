import Data.Map.Lazy (Map, empty, insert, adjust, elems, (!))
import Data.Char (isAlpha)
import Data.List (isPrefixOf, tails, stripPrefix, isInfixOf)
import Data.Maybe (fromJust)

main :: IO ()
main = print =<< getMaxRegisterVal . lines <$> getContents

getMaxRegisterVal :: [String] -> Int
getMaxRegisterVal input = maxMapVal result
    where result = processInstructions input $ mkRegisters input

mkRegisters :: [String] -> Map String Int
mkRegisters = foldl insertRegister empty

insertRegister :: Map String Int -> String -> Map String Int
insertRegister mapping xs = insert second 0 $ insert first 0 mapping
    where (first, second) = parseRegisters xs

processInstructions :: [String] -> Map String Int -> Map String Int
processInstructions input mapping = foldl processInstruction mapping input

processInstruction :: Map String Int -> String -> Map String Int
processInstruction mapping instruction
    | secondVal `operator` val = adjust (+ incrementVal) first mapping
    | otherwise = mapping
    where (first, second) = parseRegisters instruction
          secondVal = mapping ! second
          operator = parseOperator instruction
          val = read . reverse . takeWhile isNumVal $ reverse instruction
          incrementVal = parseIncrementVal instruction

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
