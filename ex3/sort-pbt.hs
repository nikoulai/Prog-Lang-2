import Test.QuickCheck

qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [y | y <- xs, y < x]
        larger  = [y | y <- xs, y >= x]

prop_same_length :: [Int] -> Bool
prop_same_length l = length l == length (qsort l)

isSorted (x : xs@(y : _)) = x <= y && isSorted xs
isSorted _ = True

prop_is_sorted :: [Int] -> Bool
prop_is_sorted l = isSorted (qsort l)

prop_sorted_already (Ordered xs) =
  qsort xs == (xs :: [Int])

main = do
  -- Using explicit generators:
  putStrLn "testing for length"
  quickCheck prop_same_length
  putStrLn "testing for sorted"
  quickCheck prop_is_sorted
  putStrLn "testing for already sorted"
  quickCheck prop_sorted_already
