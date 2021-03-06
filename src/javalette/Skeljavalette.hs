module Skeljavalette where

-- Haskell module generated by the BNF converter

import Absjavalette
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Program definitions  -> failure x


transArg :: Arg -> Result
transArg x = case x of
  Arg type' id  -> failure x


transDefinition :: Definition -> Result
transDefinition x = case x of
  Definition type' id args block  -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  Block statements  -> failure x


transStatement :: Statement -> Result
transStatement x = case x of
  SEmpty  -> failure x
  SBlock block  -> failure x
  SDeclaration type' declarations  -> failure x
  SReturn expr  -> failure x
  SReturnV  -> failure x
  SIf expr statement  -> failure x
  SIfElse expr statement0 statement  -> failure x
  SWhile expr statement  -> failure x
  SInc id  -> failure x
  SDec id  -> failure x
  SAss id expr  -> failure x
  SExpr expr  -> failure x


transDeclaration :: Declaration -> Result
transDeclaration x = case x of
  DNoInit id  -> failure x
  DInit id expr  -> failure x


transType :: Type -> Result
transType x = case x of
  TInt  -> failure x
  TDouble  -> failure x
  TBool  -> failure x
  TVoid  -> failure x
  TFun type' types  -> failure x


transExpr :: Expr -> Result
transExpr x = case x of
  ETyped type' expr  -> failure x
  EVar id  -> failure x
  EInt n  -> failure x
  EDouble d  -> failure x
  EBool lbool  -> failure x
  ECall id exprs  -> failure x
  EString str  -> failure x
  ENeg expr  -> failure x
  ENot expr  -> failure x
  EMul expr0 mulop expr  -> failure x
  EAdd expr0 addop expr  -> failure x
  EEqu expr0 equop expr  -> failure x
  ERel expr0 relop expr  -> failure x
  EAnd expr0 expr  -> failure x
  EOr expr0 expr  -> failure x


transLBool :: LBool -> Result
transLBool x = case x of
  LTrue  -> failure x
  LFalse  -> failure x


transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus  -> failure x
  Minus  -> failure x


transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times  -> failure x
  Div  -> failure x
  Mod  -> failure x


transEquOp :: EquOp -> Result
transEquOp x = case x of
  EQU  -> failure x
  NE  -> failure x


transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH  -> failure x
  LE  -> failure x
  GTH  -> failure x
  GE  -> failure x



