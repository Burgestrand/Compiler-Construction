module TypeChecker (typecheck) where

import AST
import Printer
import ErrM

import Control.Monad.State
import Data.List
import qualified Data.Map as Map

--

type Env      = [Scope]
type Scope    = Map.Map Ident Type

--
-- Typechecker
--

typecheck :: Program -> Env
typecheck (Program defs) = flip execState emptyEnv $ do
  collectDefinitions defs
  sequence_ $ map checkDef defs
  -- checkReturns defs
  

-- | Iterate through all function definitions, modifying the environment
--   by adding them and their types to it.
collectDefinitions :: [Definition] -> State Env ()
collectDefinitions defs = mapM_ finder defs
  where
    finder (Definition returns name args _) = 
      addVar name (TFun returns (map (\(Arg t _) -> t) args))

-- 
checkDef :: Definition -> State Env ()
checkDef (FuncDef t _ args (Block stms)) = do
  scope <- get
  addScope
  mapM_ (\(Arg t id) -> addVar id t)
  checkStms t stms
  put scope

checkBlock :: Type -> Block -> State Env ()
checkBlock rett (Block stms) = do s <- get
                                  pushScope
                                  checkStms rett stms
                                  put s

checkStms :: Type -> [Statement] -> State Env ()
checkStms _    []         = return ()
checkStms rett (stm:stms) = do checkStm  rett stm
                               checkStms rett stms


checkStm :: Type -> Statement -> State Env ()
checkStm rett stm = case stm of
    (SEmpty)              -> return ()
    (SBlock b)            -> checkBlock rett b
    (SDeclaration t [ds]) -> mapM_ (varDecl t) ds
    (SReturn e)           -> do t <- infer e
                                if t /= rett 
                                   then typeError stm [rett] t
                                   else return ()
    (SReturnV)            -> if TVoid /= rett 
                                then typeError stm [rett] TVoid
                                else return ()
    (SIf e tstm)          -> do t <- infer e
                                if t == TBool 
                                    then checkStm rett tstm
                                    else typeError e [TBool] t 
    (SIfElse e tstm fstm) -> do t <- infer e
                                if t == TBool 
                                    then do checkStm rett tstm
                                            checkStm rett fstm
                                    else typeError e [TBool] t
    (SWhile e stm)        -> do t <- infer e
                                if t == TBool 
                                    then do checkStm rett tstm
                                    else typeError e [TBool] t 
    (SExpr e)             -> void $ infer e
    

varDecl :: Type -> Declaration -> State Env ()
varDecl t decl = case decl of
    (DNoInit id) -> addVar id t
    (DInit id e) -> t' <- infer e
                    if t' =/ t
                        then typeError decl [t] t'
                        else addVar id t

                                    
infer :: Expr -> State Env Type
infer e = case e of  
    (EInc id)          -> do t <- lookupVar id
                             if t `elem` [TInt, TDouble] 
                                then return t
                                else typeError id [TInt, TDouble] t
    (EDec id)          -> infer (EInc id)
    (EVar id)          -> lookupVar id
    (EInt _)           -> return TInt    
    (EDouble _)        -> return TDouble 
    (EBool _)          -> return TBool
    (ECall id args)    -> do f' <- lookupVar id
                             ts' <- mapM infer args
                             case f' of
                                TFun t ts | ts /= ts' -> typeError id [TFun t ts] (TFun t ts')
                                          | otherwise -> return t
                                _ -> fail $ (printTree id ) ++ " isn't a function"
    (EString _)        -> return TString
    (ENeg e)           -> do t <- infer e
                             if t `elem` [TInt, TDouble] 
                                then return t
                                else typeError id [TInt, TDouble] t
    (ENot e)           -> do t <- infer e
                             if t `elem` [TBool] 
                                then return t
                                else typeError id [TBool] t
    (EMul e1 op e2)    -> do t1 <- infer e1
                             t2 <- infer e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return t1
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EAdd e1 op e2)    -> infer (EMul e1 MulOp e2)
    (EEqu e1 op e2)    -> do t1 <- infer e1
                             t2 <- infer e2
                             if t2 == t1 
                                then return TBool
                             else typeError e2 [t1] t2
    (ERel e1 op e2)    -> do t1 <- infer e1
                             t2 <- infer e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return TBool
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EAnd e1 e2)       -> do t1 <- infer e1
                             t2 <- infer e2
                             if t1 == TBool 
                                then if t2 == t1 
                                     then return TBool
                                     else typeError e2 [t1] t2
                                else typeError e1 [TBool] t1
    (EOr e1 e2)        -> infer (ELAnd e1 e2)
    (EAss id e)        -> do vt <- lookupVar id
                             et <- infer e
                             if et == vt 
                                then return et
                                else typeError e [vt] et
                            
                            

typeError e ts t' = fail (printTree e ++ " has type " ++ printTree t'
                    ++ " expected " ++ treeify ts)
                    where treeify []     = "a miracle" -- shouldn't happen
                          treeify [a]    = printTree a
                          treeify [a, b] = printTree a ++ " or " ++ printTree b
                          treeify (a:as) = printTree a ++ ", " ++ printTree as


--
-- Environment
--

-- | Empty environment
emptyEnv :: Env
emptyEnv = [Map.empty]

-- | Add an identifier to the current scope
addVar :: Ident -> Type -> State Env ()
addVar id t = do
  (scope:scopes) <- get
  if Map.member id scope
    then fail $ "Identifier " ++ printTree id ++ " already declared."
    else put $ (Map.insert id t scope):scopes

-- | Find an identifier in the environment
lookupVar :: Ident -> State Env Type
lookupVar id = do
  env <- get
  case find (Map.member id) env of
    Just scope -> return $ (Map.!) scope id
    Nothing    -> fail   $ "Unknown identifier " ++ printTree id

-- | Add an empty scope layer atop the environment
pushScope :: State Env ()
pushScope = modify (emptyEnv ++)
