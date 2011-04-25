module Printjavalette where

-- pretty-printer generated by the BNF converter

import Absjavalette
import Char

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString i)



instance Print Program where
  prt i e = case e of
   Program definitions -> prPrec i 0 (concatD [prt 0 definitions])


instance Print Arg where
  prt i e = case e of
   Arg type' id -> prPrec i 0 (concatD [prt 0 type' , prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Definition where
  prt i e = case e of
   Definition type' id args block -> prPrec i 0 (concatD [prt 0 type' , prt 0 id , doc (showString "(") , prt 0 args , doc (showString ")") , prt 0 block])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Block where
  prt i e = case e of
   Block statements -> prPrec i 0 (concatD [doc (showString "{") , prt 0 statements , doc (showString "}")])


instance Print Statement where
  prt i e = case e of
   SEmpty  -> prPrec i 0 (concatD [doc (showString ";")])
   SBlock block -> prPrec i 0 (concatD [prt 0 block])
   SDeclaration type' declarations -> prPrec i 0 (concatD [prt 0 type' , prt 0 declarations , doc (showString ";")])
   SReturn expr -> prPrec i 0 (concatD [doc (showString "return") , prt 0 expr , doc (showString ";")])
   SReturnV  -> prPrec i 0 (concatD [doc (showString "return") , doc (showString ";")])
   SIf expr statement -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 statement])
   SIfElse expr statement0 statement -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 statement0 , doc (showString "else") , prt 0 statement])
   SWhile expr statement -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 expr , doc (showString ")") , prt 0 statement])
   SInc id -> prPrec i 0 (concatD [prt 0 id , doc (showString "++") , doc (showString ";")])
   SDec id -> prPrec i 0 (concatD [prt 0 id , doc (showString "--") , doc (showString ";")])
   SAss id expr -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 expr , doc (showString ";")])
   SExpr expr -> prPrec i 0 (concatD [prt 0 expr , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Declaration where
  prt i e = case e of
   DNoInit id -> prPrec i 0 (concatD [prt 0 id])
   DInit id expr -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 expr])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Type where
  prt i e = case e of
   TInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TDouble  -> prPrec i 0 (concatD [doc (showString "double")])
   TBool  -> prPrec i 0 (concatD [doc (showString "boolean")])
   TVoid  -> prPrec i 0 (concatD [doc (showString "void")])
   TFun type' types -> prPrec i 0 (concatD [prt 0 type' , doc (showString "(") , prt 0 types , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Expr where
  prt i e = case e of
   ETyped type' expr -> prPrec i 8 (concatD [prt 0 type' , prt 0 expr])
   EVar id -> prPrec i 7 (concatD [prt 0 id])
   EInt n -> prPrec i 7 (concatD [prt 0 n])
   EDouble d -> prPrec i 7 (concatD [prt 0 d])
   EBool lbool -> prPrec i 7 (concatD [prt 0 lbool])
   ECall id exprs -> prPrec i 7 (concatD [prt 0 id , doc (showString "(") , prt 0 exprs , doc (showString ")")])
   EString str -> prPrec i 7 (concatD [prt 0 str])
   ENeg expr -> prPrec i 6 (concatD [doc (showString "-") , prt 7 expr])
   ENot expr -> prPrec i 6 (concatD [doc (showString "!") , prt 7 expr])
   EMul expr0 mulop expr -> prPrec i 5 (concatD [prt 5 expr0 , prt 0 mulop , prt 6 expr])
   EAdd expr0 addop expr -> prPrec i 4 (concatD [prt 4 expr0 , prt 0 addop , prt 5 expr])
   EEqu expr0 equop expr -> prPrec i 3 (concatD [prt 3 expr0 , prt 0 equop , prt 4 expr])
   ERel expr0 relop expr -> prPrec i 3 (concatD [prt 3 expr0 , prt 0 relop , prt 4 expr])
   EAnd expr0 expr -> prPrec i 2 (concatD [prt 3 expr0 , doc (showString "&&") , prt 2 expr])
   EOr expr0 expr -> prPrec i 1 (concatD [prt 2 expr0 , doc (showString "||") , prt 1 expr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print LBool where
  prt i e = case e of
   LTrue  -> prPrec i 0 (concatD [doc (showString "true")])
   LFalse  -> prPrec i 0 (concatD [doc (showString "false")])


instance Print AddOp where
  prt i e = case e of
   Plus  -> prPrec i 0 (concatD [doc (showString "+")])
   Minus  -> prPrec i 0 (concatD [doc (showString "-")])


instance Print MulOp where
  prt i e = case e of
   Times  -> prPrec i 0 (concatD [doc (showString "*")])
   Div  -> prPrec i 0 (concatD [doc (showString "/")])
   Mod  -> prPrec i 0 (concatD [doc (showString "%")])


instance Print EquOp where
  prt i e = case e of
   EQU  -> prPrec i 0 (concatD [doc (showString "==")])
   NE  -> prPrec i 0 (concatD [doc (showString "!=")])


instance Print RelOp where
  prt i e = case e of
   LTH  -> prPrec i 0 (concatD [doc (showString "<")])
   LE  -> prPrec i 0 (concatD [doc (showString "<=")])
   GTH  -> prPrec i 0 (concatD [doc (showString ">")])
   GE  -> prPrec i 0 (concatD [doc (showString ">=")])



