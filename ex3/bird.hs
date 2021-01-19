import Test.QuickCheck
import Data.Ratio


data Tree a = T a [Tree a]
 deriving Show
--1
-- instance Arbitrary a => Arbitrary (Tree a) where
--   arbitrary = do
--     t <- arbitrary
--     ts <- arbitrary
--     return (Tree t ts)
-- arbTree :: Arbitrary a => Int -> Gen (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  return (T t ts)


  ----------------------------------------------------------------------
foldTree :: ( a -> [b] -> b ) -> Tree a -> b
foldTree f (T x s) = f x (map (foldTree f) s)

sizeTree t = foldTree (\_ ys -> 1 + sum ys) t
-- sizeTree (T a []) = 1
-- sizeTree (T a b) = 1 + sum (map sizeTree b)
--
--
-- heightTree t = foldTree (\_ ys -> 1 + max ys ) t

heightTree t = foldTree (\_ ys -> 1 + if( ys==[]) then 0 else maximum ys ) t

sumTree t = foldTree (\x ys -> x +  sum ys) t
--
maxTree t = maximum $ foldTree (\x s -> x:concat s) t
--
inTree a t = a `elem` foldTree (\x s -> x:concat s) t
--
nodes t = foldTree (\x s -> x:concat s) t
--
countTree f t = length [ a | a <- foldTree (\x s -> x:concat s) t, f a ]--

leaves t = leaves' (== []) t
 where
  leaves' f t = foldTree (\x s -> if(f s ) then x:concat s else concat s) t

mapTree f t = foldTree (\ x s -> T (f x) s ) t


trimTree n t = trimTree' n t
 where
   trimTree' n (T x s) = if n==1 then (T x []) else T x (map (trimTree' (n-1)) s)

path l t = path' l t
  where
    -- path' [] (T x _) = x
    path' [] (T x s ) = x
    path' (0:tl) (T x ((T x' xs):_)) = path' tl (T x' xs)
    -- path' (h:tl) (T x []) = error "error"
    path' (h:tl) (T x ((T x' xs):[s])) = path' ((h-1):tl) (T x [s])
    path' (h:tl) (T x _) = error "error"
------------------------------------------------------------------------------
-- prop_same_length :: [Int] -> Bool
-- prop_same_length l = length l == length (qsort l)

prop_sizeTree :: (Tree a) -> Bool
prop_sizeTree t = (heightTree t) <= (sizeTree t)

prop_max_inTree::(Ord a) => (Tree a) -> Bool
prop_max_inTree t = inTree (maxTree t) t

prop_nodes_inTree::(Eq a) => (Tree a) -> Bool
prop_nodes_inTree t = foldl (&&) True (map (inTree' t) ( nodes t))
   where
     inTree' a b = inTree b a

instance Show (a -> b) where
        show a= "funcion"
-- prop_countTree_natural::Show (a->Bool) => (a->Bool)->(Tree a) -> Bool
prop_countTree_natural f t = (countTree f) t  <= sizeTree t
-- && floor (abs(n))== ceiling (abs(n))

prop_nodes ::  Eq a =>Tree a -> Bool
prop_nodes t = length ( nodes t ) == sizeTree t && if length (leaves t) == 1 && sizeTree t == 1 then True else  length (leaves t) < (sizeTree t)

prop_size_height_mapTree::(Show b)=>(a->b)->(Tree a)->Bool
prop_size_height_mapTree f t = (sizeTree t) == sizeTree( (mapTree f) t ) &&  (heightTree t )== (heightTree( (mapTree f) t ))

prop_inMapTree f n t = (inTree n t) && (inTree (f n) (mapTree f t))

prop_composition f t = (map f . nodes) t == (nodes .  mapTree f) t &&
                       (map f . leaves) t == (leaves .  mapTree f) t


--bird
-- data Tree a = T a [Tree a]
bird :: Tree  Rational
bird = T 1 [ left, right]
  where left = (mapTree (1/))  $(mapTree (+1)) bird
        right = (mapTree (+1)) $(mapTree (1/)) bird

--4.1
prop_path_trim::(Num a1, Eq a1, Eq a) => [a1]->(Tree a) -> Bool
prop_path_trim n t = (path n bird  )==( path n (trimTree (length n) bird))


path' l t = path'' l t []
  where
    -- path' [] (T x _) = x
    path'' [] (T x s ) ys = [x]++ys
    path'' (0:tl) (T x ((T x' xs):_)) ys = path'' tl (T x' xs) [x]++ys
    -- path' (h:tl) (T x []) = error "error"
    path'' (h:tl) (T x ((T x' xs):[s])) ys = path'' ((h-1):tl) (T x [s]) ys
    path'' (h:tl) (T x _) _ = error "error"


--4.2
-- prop_path_natural:: (Eq a, Ord a) => a -> Bool
prop_path_natural n = (path' (take n' zigzag) bird) == (reverse $take (n'+1) [1.0,2.0..])
  where zigzag = [1,0]++zigzag
        n' = abs(n)
--4.3
prop_fibo_path n = (map (\ x -> denominator x)(path' (take n' zeroes) bird))++[1,0] == reverse (take (n'+3) fibs)
  where zeroes = [0,0..]
        fibs = 0 : 1 : sumlists fibs (tail fibs)
          where sumlists (x:xs) (y:ys) = (x + y) : sumlists xs ys
        n' = abs(n)

-- zeroes = [0,0..]
-- fibs = 0 : 1 : sumlists fibs (tail fibs)
--     where sumlists (x:xs) (y:ys) = (x + y) : sumlists xs ys
--4.4
prop_all_rational :: Rational -> Bool
prop_all_rational n = inTree n $ trimTree ((length (findBird n)) + 1) bird

findBird q = finder q bird 0
    where finder q (T x xs) level
            | q == x = []
            | q < x  = [position] ++( finder q (xs!!position) (level+1))
            | otherwise = [position'] ++ (finder q (xs!!position') (level+1))
              where position = level `mod` 2
                    position' = (level+1) `mod` 2
