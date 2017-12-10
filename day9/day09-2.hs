main :: IO ()
main = print =<< length . removeNonGarbage <$> getContents

removeNonGarbage :: String -> String
removeNonGarbage [] = []
removeNonGarbage xs
    -- can't just check if garbage is null, because <> would give us null
    -- garbage. Instead, we have to check that no characters have been
    -- dropped, which is a no-op.
    | length garbage + length rest == length start = []
    | otherwise = garbage ++ removeNonGarbage rest
    where start = dropWhile (/= '<') xs
          (rest, garbage) = spanGarbage start

spanGarbage :: String -> (String, String)
spanGarbage [] = ([], [])
spanGarbage ('<':xs) = spanGarbage' xs []
spanGarbage _ = error "Stream didn't start with garbage!"

spanGarbage' :: String -> String -> (String, String)
spanGarbage' [] acc = ([], acc)
-- garbage ended
spanGarbage' ('>':xs) acc = (xs, acc)
-- skip cancelled character
spanGarbage' ('!':_:xs) acc = spanGarbage' xs acc
-- if no cancelled character, empty string
spanGarbage' ('!':_) acc = ([], acc)
-- default case, keep char
spanGarbage' (x:xs) acc = spanGarbage' xs (acc ++ [x])
