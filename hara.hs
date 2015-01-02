type Vector = (Double, Double, Double)

newtype Color = Color { char :: Char }

width = 80
height = 40

sphereOrigin :: Vector
sphereOrigin = (0, 0, -2)

sphereRadius :: Double
sphereRadius = 0.8

printBar =
    do
      putChar '+'
      putStr $ replicate width '-'
      putStrLn "+"

printRows :: [[Color]] -> IO ()
printRows [] = return ()
printRows (row:tail) =
    do
      printRows tail
      putChar '|'
      printColumns row
      putStrLn "|"

printColumns :: [Color] -> IO ()
printColumns [] = return ()
printColumns (color:tail) =
    do
      printColumns tail
      putChar $ char color

main =
    do
      printBar
      printRows $ traceRows (height `quot` 2) []
      printBar

traceRows :: Int -> [[Color]] -> [[Color]]
traceRows y rows
    | length rows' < height = traceRows (y - 1) rows'
    | otherwise = rows'
    where rows' = traceColumns y (width `quot` 2) []:rows

traceColumns :: Int -> Int -> [Color] -> [Color]
traceColumns y x columns
    | length columns' < width = traceColumns y (x - 1) columns'
    | otherwise = columns'
    where columns' = traceDot y x:columns

traceDot :: Int -> Int -> Color
traceDot row column =
    case traceRay x y of
      Just _  -> Color '#'
      Nothing -> Color ' '
    where x = (fromIntegral column - 0.5) / fromIntegral width
          y = (fromIntegral row - 0.5) / fromIntegral height

traceRay :: Double -> Double -> Maybe Double
traceRay x y =
    if d >= 0
    then Just 1
    else Nothing
        where (rx, ry, rz) = normalize (x, y, 1)
              (sx, sy, sz) = sphereOrigin
              b = negate (rx * sx + ry * sy + rz * sz) * 2
              c = sx^2 + sy^2 + sz^2 - sphereRadius^2
              d = b^2 - c * 4

normalize :: Vector -> Vector
normalize (x, y, z) = (x * f, y * f, z * f)
    where f = 1 / sqrt (x^2 + y^2 + z^2)
