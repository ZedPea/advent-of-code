import Data.Char (isAlpha)
import Data.Map.Lazy (Map, empty, insertWith, toAscList)
import qualified Data.Map.Lazy as M (filter)

data Program = Program {
    name :: String,
    subPrograms :: Maybe [String]
} deriving (Show, Ord, Eq)

main :: IO ()
main = putStrLn =<< baseProgram . map parseProgram . lines <$> getContents

parseProgram :: String -> Program
parseProgram input = Program name' $ subPrograms' rest'
    where (name', rest) = span (/= ' ') input
          rest' = dropWhile (not . isAlpha) rest
          subPrograms' [] = Nothing
          subPrograms' xs = Just . words $ filter (/= ',') xs

baseProgram :: [Program] -> String
baseProgram progs = fst . head . toAscList $ M.filter (== 0) mapping
    where mapping = foldl insertProg empty progs

insertSubPrograms :: Map String Int -> Program -> Map String Int
insertSubPrograms mapping prog = case subPrograms prog of
    Nothing -> mapping
    Just subprogs -> foldl (\acc x -> insertWith (+) x 1 acc) mapping subprogs

insertProg :: Map String Int -> Program -> Map String Int
insertProg mapping prog = flip insertSubPrograms prog $ 
                          insertWith (+) (name prog) 0 mapping
