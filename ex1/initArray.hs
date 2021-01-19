import Data.Array as Array
initialArray n = Array.array bounds [((i,j), initArray i j ) | (i,j) <- Array.range bounds ]
  where bounds = ((0,0),(n,n))
initArray:: Int->Int->Int
initArray i j
  | i == j = 1
  | otherwise = 0
