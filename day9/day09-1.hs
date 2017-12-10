import Data.List (span)

main :: IO ()
main = print =<< score <$> getContents

score :: String -> Int
score = snd . foldl updScore (0, 0) . removeGarbage

updScore :: (Int, Int) -> Char -> (Int, Int)
updScore (nesting, acc) '{' = (nesting+1, acc)
updScore (nesting, acc) '}' = (nesting-1, acc+nesting)
updScore acc _ = acc

removeGarbage :: String -> String
removeGarbage [] = []
removeGarbage xs
    | null filtered = start
    | otherwise = start ++ removeGarbage filtered
    where (start, rest) = span (/= '<') xs
          filtered = dropWhileGarbage rest

dropWhileGarbage :: String -> String
dropWhileGarbage [] = []
dropWhileGarbage ('<':xs) = dropWhileGarbage' xs
dropWhileGarbage _ = error "Stream didn't start with garbage!"

dropWhileGarbage' :: String -> String
dropWhileGarbage' [] = []
-- garbage ended
dropWhileGarbage' ('>':xs) = xs
-- skip cancelled character
dropWhileGarbage' ('!':_:xs) = dropWhileGarbage' xs
-- if no cancelled character, empty string
dropWhileGarbage' ('!':_) = []
-- default case, just drop char
dropWhileGarbage' (_:xs) = dropWhileGarbage' xs
