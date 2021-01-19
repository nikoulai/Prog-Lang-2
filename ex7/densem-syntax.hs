import Data.Char
import Text.Read
import Text.Read.Lex

-- Syntax

type Var = String

data C = Cskip | Cassign Var E | Cseq C C | Cfor E C | Cif E C C | Cwhile E C
data E = Ezero | Esucc E | Epred E | Eif E E E | Evar Var
       | Etrue | Efalse | Elt E E | Eeq E E | Enot E
       | Econs E E | Ehd E | Etl E

-- Pretty-printing

instance Show E where
  showsPrec p Ezero = ("0" ++)
  showsPrec p (Esucc n) = ("succ " ++) . showsPrec 2 n
  showsPrec p (Epred n) = ("pred " ++) . showsPrec 2 n
  showsPrec p (Eif e e1 e2) =
    showParen (p > 0) $
    ("if " ++) . showsPrec 1 e . (" then " ++) . showsPrec 1 e1 .
                                 (" else " ++) . showsPrec 0 e2
  showsPrec p (Evar x) = (x ++)
  showsPrec p Etrue = ("true" ++)
  showsPrec p Efalse = ("false" ++)
  showsPrec p (Elt e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" < " ++) . showsPrec 2 e2
  showsPrec p (Eeq e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" = " ++) . showsPrec 2 e2
  showsPrec p (Enot e) =
    showParen (p > 2) $
    ("not " ++) . showsPrec 2 e
  showsPrec p (Econs e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" : " ++) . showsPrec 1 e2
  showsPrec p (Ehd e) =
    showParen (p > 2) $
    ("hd " ++) . showsPrec 2 e
  showsPrec p (Etl e) =
    showParen (p > 2) $
    ("tl " ++) . showsPrec 2 e

instance Show C where
  showsPrec p Cskip = ("skip" ++)
  showsPrec p (Cassign x e) = (x ++) . (" := " ++) . showsPrec 0 e
  showsPrec p (Cseq c1 c2) =
    showParen (p > 0) $
    showsPrec 1 c1 . ("; " ++) . showsPrec 0 c2
  showsPrec p (Cfor e c) =
    showParen (p > 1) $
    ("for " ++) . showsPrec 1 e . (" do " ++) . showsPrec 1 c
  showsPrec p (Cif e c1 c2) =
    showParen (p > 1) $
    ("if " ++) . showsPrec 1 e . (" then " ++) . showsPrec 2 c1 .
                                 (" else " ++) . showsPrec 1 c2
  showsPrec p (Cwhile e c) =
    showParen (p > 1) $
    ("while " ++) . showsPrec 1 e . (" do " ++) . showsPrec 1 c

-- Parsing

isVar x = all isAlpha x && not (x `elem` keywords)
  where keywords = ["zero", "succ", "true", "not", "skip",
                    "for", "if", "then", "else", "while", "do",
                    "true", "false", "hd", "tl"]

when True p = p
when False _ = fail "when failed"

instance Read E where
  readPrec = parens $
             (prec 1 $ do
                e1 <- step readPrec
                Symbol ":" <- lexP
                e2 <- readPrec
                return (Econs e1 e2)) <++
             (prec 1 $ do
                e1 <- step readPrec
                Symbol "<" <- lexP
                e2 <- step readPrec
                return (Elt e1 e2)) <++
             (prec 1 $ do
                e1 <- step readPrec
                Symbol "=" <- lexP
                e2 <- step readPrec
                return (Eeq e1 e2)) <++
             (do
                Number n <- lexP
                when (numberToInteger n == Just 0) $ do
                  return Ezero) <++
             (prec 2 $ do
                Ident "succ" <- lexP
                e <- readPrec
                return (Esucc e)) <++
             (prec 2 $ do
                Ident "pred" <- lexP
                e <- readPrec
                return (Epred e)) <++
             (prec 0 $ do
                Ident "if" <- lexP
                e <- step readPrec
                Ident "then" <- lexP
                e1 <- step readPrec
                Ident "else" <- lexP
                e2 <- readPrec
                return (Eif e e1 e2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  return (Evar x)) <++
             (do
                Ident "true" <- lexP
                return Etrue) <++
             (do
                Ident "false" <- lexP
                return Efalse) <++
             (prec 2 $ do
                Ident "not" <- lexP
                e <- readPrec
                return (Enot e)) <++
             (prec 2 $ do
                Ident "hd" <- lexP
                e <- readPrec
                return (Ehd e)) <++
             (prec 2 $ do
                Ident "tl" <- lexP
                e <- readPrec
                return (Etl e))

instance Read C where
  readPrec = parens $
             (prec 0 $ do
                c1 <- step readPrec
                Punc ";" <- lexP
                c2 <- readPrec
                return (Cseq c1 c2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol ":=" <- lexP
                  e <- reset readPrec
                  return (Cassign x e)) <++
             (do
                Ident "skip" <- lexP
                return Cskip) <++
             (prec 1 $ do
                Ident "if" <- lexP
                e <- readPrec
                Ident "then" <- lexP
                c1 <- step readPrec
                Ident "else" <- lexP
                c2 <- readPrec
                return (Cif e c1 c2)) <++
             (prec 1 $ do
                Ident "for" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cfor e c)) <++
             (prec 1 $ do
                Ident "while" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cwhile e c))

-- Main function: parsing a statement and pretty-printing

main = do  input <- getContents
           let c :: C
               c = read input
           print c
