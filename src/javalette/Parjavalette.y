-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parjavalette where
import Absjavalette
import Lexjavalette
import ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '!' { PT _ (TS _ 1) }
 '!=' { PT _ (TS _ 2) }
 '%' { PT _ (TS _ 3) }
 '&&' { PT _ (TS _ 4) }
 '(' { PT _ (TS _ 5) }
 ')' { PT _ (TS _ 6) }
 '*' { PT _ (TS _ 7) }
 '+' { PT _ (TS _ 8) }
 '++' { PT _ (TS _ 9) }
 ',' { PT _ (TS _ 10) }
 '-' { PT _ (TS _ 11) }
 '--' { PT _ (TS _ 12) }
 '/' { PT _ (TS _ 13) }
 ';' { PT _ (TS _ 14) }
 '<' { PT _ (TS _ 15) }
 '<=' { PT _ (TS _ 16) }
 '=' { PT _ (TS _ 17) }
 '==' { PT _ (TS _ 18) }
 '>' { PT _ (TS _ 19) }
 '>=' { PT _ (TS _ 20) }
 'boolean' { PT _ (TS _ 21) }
 'double' { PT _ (TS _ 22) }
 'else' { PT _ (TS _ 23) }
 'false' { PT _ (TS _ 24) }
 'if' { PT _ (TS _ 25) }
 'int' { PT _ (TS _ 26) }
 'return' { PT _ (TS _ 27) }
 'true' { PT _ (TS _ 28) }
 'void' { PT _ (TS _ 29) }
 'while' { PT _ (TS _ 30) }
 '{' { PT _ (TS _ 31) }
 '||' { PT _ (TS _ 32) }
 '}' { PT _ (TS _ 33) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListDefinition { Program $1 } 


Arg :: { Arg }
Arg : Type Ident { Arg $1 $2 } 


ListArg :: { [Arg] }
ListArg : {- empty -} { [] } 
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }


Definition :: { Definition }
Definition : Type Ident '(' ListArg ')' Block { Definition $1 $2 $4 $6 } 


ListDefinition :: { [Definition] }
ListDefinition : Definition { (:[]) $1 } 
  | Definition ListDefinition { (:) $1 $2 }


Block :: { Block }
Block : '{' ListStatement '}' { Block (reverse $2) } 


ListStatement :: { [Statement] }
ListStatement : {- empty -} { [] } 
  | ListStatement Statement { flip (:) $1 $2 }


Statement :: { Statement }
Statement : ';' { SEmpty } 
  | Block { SBlock $1 }
  | Type ListDeclaration ';' { SDeclaration $1 $2 }
  | 'return' Expr ';' { SReturn $2 }
  | 'return' ';' { SReturnV }
  | 'if' '(' Expr ')' Statement { SIf $3 $5 }
  | 'if' '(' Expr ')' Statement 'else' Statement { SIfElse $3 $5 $7 }
  | 'while' '(' Expr ')' Statement { SWhile $3 $5 }
  | Ident '++' ';' { SInc $1 }
  | Ident '--' ';' { SDec $1 }
  | Ident '=' Expr ';' { SAss $1 $3 }
  | Expr ';' { SExpr $1 }


Declaration :: { Declaration }
Declaration : Ident { DNoInit $1 } 
  | Ident '=' Expr { DInit $1 $3 }


ListDeclaration :: { [Declaration] }
ListDeclaration : Declaration { (:[]) $1 } 
  | Declaration ',' ListDeclaration { (:) $1 $3 }


Type :: { Type }
Type : 'int' { TInt } 
  | 'double' { TDouble }
  | 'boolean' { TBool }
  | 'void' { TVoid }


ListType :: { [Type] }
ListType : {- empty -} { [] } 
  | Type { (:[]) $1 }
  | Type ',' ListType { (:) $1 $3 }



Expr7 :: { Expr }
Expr7 : Ident { EVar $1 } 
  | Integer { EInt $1 }
  | Double { EDouble $1 }
  | LBool { EBool $1 }
  | Ident '(' ListExpr ')' { ECall $1 $3 }
  | String { EString $1 }
  | '(' Expr ')' { $2 }


Expr6 :: { Expr }
Expr6 : '-' Expr7 { ENeg $2 } 
  | '!' Expr7 { ENot $2 }
  | Expr7 { $1 }


Expr5 :: { Expr }
Expr5 : Expr5 MulOp Expr6 { EMul $1 $2 $3 } 
  | Expr6 { $1 }


Expr4 :: { Expr }
Expr4 : Expr4 AddOp Expr5 { EAdd $1 $2 $3 } 
  | Expr5 { $1 }


Expr3 :: { Expr }
Expr3 : Expr3 EquOp Expr4 { EEqu $1 $2 $3 } 
  | Expr3 RelOp Expr4 { ERel $1 $2 $3 }
  | Expr4 { $1 }


Expr2 :: { Expr }
Expr2 : Expr3 '&&' Expr2 { EAnd $1 $3 } 
  | Expr3 { $1 }


Expr1 :: { Expr }
Expr1 : Expr2 '||' Expr1 { EOr $1 $3 } 
  | Expr2 { $1 }


Expr :: { Expr }
Expr : Expr1 { $1 } 


ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] } 
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }


LBool :: { LBool }
LBool : 'true' { LTrue } 
  | 'false' { LFalse }


AddOp :: { AddOp }
AddOp : '+' { Plus } 
  | '-' { Minus }


MulOp :: { MulOp }
MulOp : '*' { Times } 
  | '/' { Div }
  | '%' { Mod }


EquOp :: { EquOp }
EquOp : '==' { EQU } 
  | '!=' { NE }


RelOp :: { RelOp }
RelOp : '<' { LTH } 
  | '<=' { LE }
  | '>' { GTH }
  | '>=' { GE }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

