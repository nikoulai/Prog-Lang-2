import DensemSyntax

import Data.Typeable
import Debug.Trace
-- syntax

type S = Var -> E

data DemVar = DemCon DemVar DemVar | DemInt Integer | DemBoo Bool
 deriving (Show)


instance (Eq E) where
    Etrue ==  Etrue  = True
    Efalse == Efalse = False

semC :: C -> S -> S
semC Cskip s = trace ("Cskip"++" \"\" ") $ s
semC (Cassign x n) s = trace ("Cassign s="++"x="++(show x)++"n="++(show n)++" \"\" ") $ update s x ( z)
  where (y,z) = (semE n s)
semC (Cseq c1 c2) s = trace ("Cseq" ++ " c1="++(show c1)++" c2="++(show c2)++" \"\" ") $semC c2 (semC c1 s)
semC (Cfor n c) s = expon i (semC c) s
  where (_,i) = semE n s
semC (Cif b c1 c2) s |(first (semE b s))==(Etrue)  = semC c1 s
                     | otherwise = semC c2 s
semC (Cwhile b c) s =  fix bigF s
  where bigF f s | (first (semE b s))   == (Etrue)  = f (semC c s)
                 | otherwise = s

semE :: E -> S -> (E,Integer)
semE Etrue s =   (Etrue,0)
semE Efalse s =  (Efalse,0)
semE (Elt n1 n2) s = if (x < y) then (Etrue,0) else (Efalse,0)
  where (_,x) = (semE n1 s)
        (_,y) = (semE n2 s)
semE (Eeq n1 n2) s = if ( x == y) then (Etrue,0) else (Efalse,0)
  where (_,x) = (semE n1 s)
        (_,y) = (semE n2 s)
-- ((semE n1 s) == (semE n2 s))
semE (Enot b) s = (Enot x,0)
  where (x,_) = (semE b s)
semE Ezero s = (Ezero,0)
semE (Evar x) s =  (s x,0)
-- semE (Epred n ) (s) = (((semE n s)-1))
  -- where (x) =(semE n s)
semE (Epred n ) (s) =  (Etrue,x - 1)
  where (_,x) = (semE n s)
semE (Esucc n ) (s) =  (Efalse,x + 1)
  where (_,x) = (semE n s)
-- semE (Esucc n ) s = semE n s + 1
semE (Eif b n1 n2 ) s | (first (semE b s)) == (Etrue) = semE n1 s
                      | otherwise = semE n2 s
semE (Econs e1 e2) s = (Econs x y,0)
  where (x,_) = (semE e1 s)
        (y,_) = (semE e2 s)
semE (Ehd e) s = (a,0)
  where (Econs a b,_) = semE e s
semE (Etl e) s = (b,0)
  where (Econs a b,_) = semE e s
-- auxiliary functions
first (x,y) = x

expon ( 0) f = id
expon ( n) f = f . expon ((n-1)) f


update s x n y | x == y    = trace ("update" ++ " x="++(show x)++" y="++(show y)++" n="++(show n)++" \"\" ") $n
               | otherwise = s y

-- example

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
