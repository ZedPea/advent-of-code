main :: IO ()
main = do
    input <- readLn
    let circle = findCircle input 1
        sideLen = findLen input 1
        -- have to add the circle length to see our offset from middle
        start = (input + circle - 1) `mod` (sideLen - 1)
        -- two options, one is potentially nearer
        offset = min start (sideLen - 1 - start)
        final = circle + offset
    print final

findCircle :: Int -> Int -> Int
findCircle n x
    | x^2 >= n = 0
    | otherwise = 1 + findCircle n (x+2)

findLen :: Int -> Int -> Int
findLen n x
    | x^2 >= n = x
    | otherwise = findLen n (x+2)
