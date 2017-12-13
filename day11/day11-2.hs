import Data.List.Split (wordsBy)
import Control.Arrow ((***))
import Control.Monad (join)

data Dir = NorthWest
         | North
         | NorthEast
         | SouthEast
         | South
         | SouthWest

-- skip the \n in input with init
main :: IO ()
main = print =<< shortestDistance . map parseDir . wordsBy (== ',') . init <$> getContents

parseDir :: String -> Dir
parseDir "nw" = NorthWest
parseDir "n" = North
parseDir "ne" = NorthEast
parseDir "se" = SouthEast
parseDir "s" = South
parseDir "sw" = SouthWest

shortestDistance :: [Dir] -> Int
shortestDistance = maximum . map (round . uncurry (+) . join (***) abs) . scanl adjustDistance (0, 0)

adjustDistance :: (Double, Double) -> Dir -> (Double, Double)
adjustDistance (x, y) NorthWest = (x - 0.5, y + 0.5)
adjustDistance (x, y) North     = (x,       y + 1.0)
adjustDistance (x, y) NorthEast = (x + 0.5, y + 0.5)
adjustDistance (x, y) SouthEast = (x + 0.5, y - 0.5)
adjustDistance (x, y) South     = (x,       y - 1.0)
adjustDistance (x, y) SouthWest = (x - 0.5, y - 0.5)
