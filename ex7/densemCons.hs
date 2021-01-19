{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

import DensemSyntax

import Data.Typeable
import Debug.Trace
-- syntax


--type Var = String

{-data C = Cskip | Cassign Var E | Cseq C C | Cfor E C | Cif E C C
               | Cwhile E C
  deriving (Show)


data E = Ezero | Esucc E | Epred E | Eif E E E | Evar Var | Etrue | Efalse | Elt E E | Eeq E E | Enot E
  deriving (Show)
-}
data ConsElement = Integ Integer | Boo Bool
  deriving (Show)

instance (Eq ConsElement) where
    (Integ a ) ==  (Integ b) = a == b
    (Boo a)    ==  (Boo b)   = a == b

instance (Ord ConsElement) where
       (Integ a ) `compare`  (Integ b) = a `compare` b

instance Num DemVar where
  (+) (DemInt d1) (DemInt d2) = DemInt (d1+d2)
  (+) _ _ = error "+ between different types"
  (*) _ _ = error "* now supported"
  (abs) _ = error "abs not supported"
  (signum) _ = error "signum not supported"
  (-) _ = error ""
  fromInteger n = let a = (fromInteger n) in (DemInt a)

data ConsCell   = Cons ConsElement ConsElement
  deriving (Show)

instance (Eq ConsCell) where
    (Cons a b) == (Cons c d) = a==c && b==d
instance (Ord ConsCell) where
       (Cons a _ ) `compare` (Cons c _) = a `compare` c
       -- && b<d

data DemVar = DemCon DemVar DemVar | DemInt Integer | DemBoo Bool

instance Show DemVar where
 showsPrec p (DemCon e1 e2) =
   showParen (p > 1) $
   showsPrec 2 e1 . (" : " ++) . showsPrec 1 e2
 showsPrec p (DemInt x) = ((show x) ++)
 showsPrec p (DemBoo x) = if x then ("true" ++) else ("false" ++)
 -- (:)::Evar->Evar->Evar
 -- data Evar = Integer | Bool | Evar:Evar


instance (Eq DemVar) where
   (DemBoo a) ==  (DemBoo b) = a == b
   (DemInt a) ==  (DemInt b) = a == b
   (DemCon a b) ==  (DemCon c d) = a == c && b == d
instance (Ord DemVar) where
   (DemBoo a) `compare`  (DemBoo b) = a `compare` b
   (DemInt a) `compare`  (DemInt b) = a `compare` b
   (DemCon a _) `compare`  (DemCon c  _) = a `compare` c

-- semantic domains


type S = Var -> DemVar

-- semantic functions

semD :: Bool -> ConsCell
semD s = if s then Cons (Boo True) (Integ 5) else Cons (Boo True) (Integ 9)



semC :: C -> S -> S
semC Cskip s = s
semC (Cassign x n) s = update s x (semE n s)
semC (Cseq c1 c2) s = semC c2 (semC c1 s)
semC (Cfor n c) s = expon i (semC c) s
  where i = semE n s
semC (Cif b c1 c2) s | semE b s ==(DemBoo True)  = semC c1 s
                     | otherwise = semC c2 s
semC (Cwhile b c) s =  fix bigF s
  where bigF f s | semE b s == (DemBoo True)  = f (semC c s)
                 | otherwise = s

semE :: E -> S -> DemVar
semE Etrue s =  DemBoo True
semE Efalse s = DemBoo False
semE (Elt n1 n2) s = DemBoo ((semE n1 s) < (semE n2 s))
semE (Eeq n1 n2) s = DemBoo ((semE n1 s) == (semE n2 s))
-- ((semE n1 s) == (semE n2 s))
semE (Enot b) s = notDem (semE b s)
semE Ezero s =(DemInt 0)
semE (Evar x) s =  s x
semE (Epred n ) (s) = (DemInt (x-1))
  where (DemInt x) =(semE n s)
--semE (Esucc n ) s = semE n s + 1
semE (Esucc n ) (s) = semE n s + (DemInt 1)
semE (Eif b n1 n2 ) s | semE b s == (DemBoo True) = semE n1 s
                      | otherwise = semE n2 s
semE (Econs e1 e2) s = DemCon (semE e1 s) (semE e2 s)
semE (Ehd e) s = a
  where DemCon a b = semE e s
semE (Etl e) s = b
  where DemCon a b = semE e s
-- auxiliary functions
notDem:: (DemVar)->(DemVar)
notDem (DemBoo a) = (DemBoo (not a))

expon (DemInt 0) f = id
expon (DemInt n) f = f . expon (DemInt (n-1)) f


update s x n y | x == y    = n
               | otherwise = s y



makeN 0 = Ezero
makeN n = Esucc (makeN (n-1))

s0 x = error ("not initialized variable " ++ x)

run c = print ((semC c s0 "result"))
{-
ex0 = Cassign "result" (makeN 42)

ex1 = Cseq (Cassign "result" Ezero)
           (Cfor (makeN 6) (
              Cfor (makeN 7) (
                Cassign "result" (Esucc (Evar "result"))
              )
           ))

ex2 = Cseq (Cassign "x" (makeN 42))
      (Cseq (Cassign "result" Ezero)
            (Cwhile (Elt Ezero (Evar "x"))
              (Cseq (Cassign "x" (Epred (Evar "x")))
                    (Cassign "result" (Esucc (Evar "result"))))))
-}
fix f = f (fix f)
{-main = do
  input <- getContents
  let c :: C
      c = read input
      -- c = ex4
  -- print c
  -- print c
  run c
  -- print (run ex2)
  -- -}
main = do
  input <- getContents
  let c :: C
      c = read input
  run c
