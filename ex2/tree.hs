import Debug.Trace

data Tree a = T a [Tree a] deriving Show


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
