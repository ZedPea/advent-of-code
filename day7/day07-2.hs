import Data.Char (isDigit)
import Data.Tree (Tree(..))
import Data.Map.Lazy (Map, empty, insertWith, toAscList)
import Data.Maybe (fromJust)
import Data.List (find, groupBy, sortBy, nub)
import Data.Function (on)
import qualified Data.Map.Lazy as M (filter)

data Program = Program {
    name :: String,
    weight :: Int,
    subPrograms :: Maybe [String]
} deriving (Show, Ord, Eq)

type Weight = Int

main :: IO ()
main = print =<< balanceTree . toTree . map parseProgram . lines <$> getContents

parseProgram :: String -> Program
parseProgram input = Program name' (read weight') (subPrograms' rest')
    where (name', _:_:rest) = span (/= ' ') input -- skip space and bracket
          (weight', _:rest') = span isDigit rest -- skip end bracket
          subPrograms' [] = Nothing
          subPrograms' xs = Just . words . filter (/= ',') $ drop 4 xs 

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

toTree :: [Program] -> Tree Weight
toTree allProgs = toTree' allProgs (baseProgram allProgs)

toTree' :: [Program] -> String -> Tree Weight
toTree' allProgs progName' = case subProgs of
    Nothing -> Node progData []
    Just subs -> Node progData $ map (toTree' allProgs) subs
    where requestedProg = findProg progName' allProgs
          progData = weight requestedProg
          subProgs = subPrograms requestedProg

findProg :: String -> [Program] -> Program
findProg needle progs = fromJust $ find (\x -> name x == needle) progs

calculateWeight :: Tree Weight -> Int
calculateWeight tree = rootLabel tree + 
                       (sum . map calculateWeight $ subForest tree)

balanceTree :: Tree Weight -> Int
balanceTree tree = balanceTree' tree 0

balanceTree' :: Tree Weight -> Int -> Int
balanceTree' tree expected
    | isBalanced tree = expected - subSum
    | otherwise = balanceTree' incorrectTree validWeight
    where (incorrectTree, validWeight) = findIncorrect $ subForest tree
          subSum = sum . map calculateWeight $ subForest tree

isBalanced :: Tree Weight -> Bool
isBalanced tree = allTheSame allWeights
    where allWeights = map calculateWeight $ subForest tree

-- sort them by weight, then group into equal weights, sort them by length,
-- and take the first one, which is the one with the shortest length, i.e.
-- only this one has a different weight. We also return the valid
-- weight of a correct tree.
findIncorrect :: [Tree Weight] -> (Tree Weight, Int)
findIncorrect trees = (head $ head xs, calculateWeight . head $ xs !! 1)
    where groupByWeight = groupBy ((==) `on` calculateWeight)
          sortByLength = sortBy (compare `on` length)
          sortByWeight = sortBy (compare `on` calculateWeight)
          xs = sortByLength . groupByWeight $ sortByWeight trees

allTheSame :: Eq a => [a] -> Bool
allTheSame [] = True
allTheSame xs = length (nub xs) == 1
