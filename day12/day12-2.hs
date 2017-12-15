import Data.List (find)
import Data.List.Split (splitOneOf)
import Data.Set (Set, fromList, union, member, size)
import qualified Data.Set as S (intersection)

main :: IO ()
main = print =<< length . combineSets . parse <$> getContents
    where splitF = filter (/= []) . splitOneOf ",<-> "
          parse = map (fromList . map read . splitF) . lines

combineSets :: [Set Int] -> [Set Int]
combineSets allSets = foldl combineSets' allSets allSets

combineSets' :: [Set Int] -> Set Int -> [Set Int]
combineSets' [] set = [set]
combineSets' (x:xs) set = case combineSet set x of
    Just newSet -> combineSets' xs newSet
    Nothing -> x : combineSets' xs set

combineSet :: Set Int -> Set Int -> Maybe (Set Int)
combineSet a b
    | null $ a `S.intersection` b = Nothing
    | otherwise = Just (a `union` b)
