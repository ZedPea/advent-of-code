import Data.Map.Lazy (Map, empty, insert, adjust, elems, (!))
import Data.List (isInfixOf)

main :: IO ()
main = print =<< getMaxRegisterVal . lines <$> getContents

getMaxRegisterVal :: [String] -> Int
getMaxRegisterVal input = processInstructions $ mkRegisters input
    where mkRegisters = foldl insertRegister empty
          processInstructions mapping = maximum . elems $
            foldl processInstruction mapping input

insertRegister :: Map String Int -> String -> Map String Int
insertRegister mapping xs = insert secondReg 0 $ insert firstReg 0 mapping
    where (firstReg, _, secondReg, _, _) = parseInstruction xs

processInstruction :: Map String Int -> String -> Map String Int
processInstruction mapping instruction
    | (mapping ! secondReg) `operator` secondNum = newMapping
    | otherwise = mapping
    where newMapping = adjust (+firstNum) firstReg mapping

          (firstReg, firstNum, secondReg, operator, secondNum) 
            = parseInstruction instruction

parseOperator :: (Ord a) => String -> (a -> a -> Bool)
parseOperator ">=" = (>=)
parseOperator "<=" = (<=)
parseOperator "==" = (==)
parseOperator "!=" = (/=)
parseOperator ">"  = (>)
parseOperator "<"  = (<)

parseInstruction :: String -> (String, Int, String, Int -> Int -> Bool, Int)
parseInstruction xs = (firstReg, firstNum, secondReg, operator, secondNum)
    where split = words xs
          firstReg = head split

          firstNum
            | "inc" `isInfixOf` xs = read $ split !! 2
            | otherwise = -(read $ split !! 2)

          secondReg = split !! 4
          operator = parseOperator $ split !! 5
          secondNum = read $ split !! 6
