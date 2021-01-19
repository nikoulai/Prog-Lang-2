import Test.QuickCheck

-- | Write a function that takes a list of integers and counts how many
-- | of them are strictly larger than all the numbers following them.

-- | A naive implementation, just following the spec -- O(n^2)
naive [] = 0
naive (x : xs) = if all (x >) xs then 1 + c else c
  where c = naive xs

-- | A more clever, greedy implementation -- O(n)
wow [] = 0
wow l = walk x xs 1
  where (x : xs) = reverse l
        walk sofar [] count = count
        walk sofar (x : xs) count
          | x <= sofar = walk sofar xs count
          | otherwise  = walk x xs (count + 1)

-- | Test that the two implementations are equivalent.

prop_equivalent :: [Int] -> Bool
prop_equivalent l = naive l == wow l

main = do
  quickCheckWith stdArgs { maxSuccess = 1000000 } prop_equivalent
