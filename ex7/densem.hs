-- syntax

type Var = String

data C = Cskip | Cassign Var N | Cseq C C | Cfor N C | Cif B C C
               | Cwhile B C
  deriving (Show)

data N = Nzero | Nsucc N | Npred N | Nif B N N | Nvar Var
  deriving (Show)

data B = Btrue | Bfalse | Blt N N | Beq N N | Bnot B
  deriving (Show)

-- semantic domains

type S = Var -> Integer

-- semantic functions

semC :: C -> S -> S
semC Cskip s = s
semC (Cassign x n) s = update s x (semN n s)
semC (Cseq c1 c2) s = semC c2 (semC c1 s)
semC (Cfor n c) s = expon i (semC c) s
  where i = semN n s
semC (Cif b c1 c2) s | semB b s  = semC c1 s
                     | otherwise = semC c2 s
semC (Cwhile b c) s = fix bigF s
  where bigF f s | semB b s  = f (semC c s)
                 | otherwise = s

semN :: N -> S -> Integer
semN Nzero s = 0
semN (Nsucc n) s = semN n s + 1
semN (Npred n) s = semN n s - 1
semN (Nif b n1 n2) s | semB b s  = semN n1 s
                     | otherwise = semN n2 s
semN (Nvar x) s = s x

semB :: B -> S -> Bool
semB Btrue s = True
semB Bfalse s = False
semB (Blt n1 n2) s = semN n1 s < semN n2 s
semB (Beq n1 n2) s = semN n1 s == semN n2 s
semB (Bnot b) s = not (semB b s)

-- auxiliary functions

expon 0 f = id
expon n f = f . expon (n-1) f

update s x n y | x == y    = n
               | otherwise = s y

-- example

makeN 0 = Nzero
makeN n = Nsucc (makeN (n-1))

s0 x = error ("not initialized variable " ++ x)

run c = print (semC c s0 "result")

ex0 = Cassign "result" (makeN 42)

ex1 = Cseq (Cassign "result" Nzero)
           (Cfor (makeN 6) (
              Cfor (makeN 7) (
                Cassign "result" (Nsucc (Nvar "result"))
              )
           ))

ex2 = Cseq (Cassign "x" (makeN 42))
      (Cseq (Cassign "result" Nzero)
            (Cwhile (Blt Nzero (Nvar "x"))
              (Cseq (Cassign "x" (Npred (Nvar "x")))
                    (Cassign "result" (Nsucc (Nvar "result"))))))

fix f = f (fix f)

main = do
  input <- getContents
  let c :: C
      c = read input
      -- c = ex4
  -- print c
  -- print c
  run c
