module TypeChecker where

import AST
import Printer
import ErrM
import Env

import Control.Monad




typecheck :: Program -> Err ()
typecheck (Program defs) = do env <- collectDefs emptyEnv defs
                              sequence_ $ map (checkDef env) defs


collectDefs :: Env -> [Definition] -> Err Env
collectDefs env []                           = return env
collectDefs env ((FuncDef t id args _):defs) = 
    do  env' <- addAny env id (Sign t args)
        collectDefs env' defs


{-
void :: Monad m => m a -> m ()
void a = do _ <- a
            return ()


    


checkDef :: Env -> Definition -> Err ()
checkDef env (FuncDef t _ args stms) = do env' <- addArgs (addScope env) args
                                          void $ checkStms t env' stms
    where addArgs env []                = return env
          addArgs env ((Arg t id):args) = do env' <- addAny env id (TType t)
                                             addArgs env' args


checkStms :: Type -> Env -> [Statement] -> Err Env
checkStms _    env []         = return env
checkStms rett env (stm:stms) = do env' <- checkStm rett env stm
                                   checkStms rett env' stms

checkStm :: Type -> Env -> Statement -> Err Env
checkStm rett env stm = case stm of
    (SExpr e)             -> do _ <- checkExpr env e
                                return env
    (SVarDecl d)          -> checkVarDecl env d
    (SBlock stms)         -> do _ <- checkStms rett (addScope env) stms
                                return env
    (SReturn e)           -> do t <- checkExpr env e
                                if t == rett 
                                    then return env
                                    else typeError stm [rett] t
    (SWhile e stm)        -> do t <- checkExpr env e
                                if t == TBool 
                                    then do _ <- checkStm rett (addScope env) stm
                                            return env
                                    else typeError e [TBool] t
    (SIfElse e tstm fstm) -> do t <- checkExpr env e
                                if t == TBool 
                                    then do _ <- checkStm rett (addScope env) tstm
                                            _ <- checkStm rett (addScope env) fstm
                                            return env
                                    else typeError e [TBool] t
                                    
checkExpr :: Env -> Expr -> Err Type
checkExpr env e = case e of
    (EInt _)           -> return TInt    
    (EDouble _)        -> return TDouble 
    (EBool _)          -> return TBool  
    (EIdent id)        -> do t <- lookupVar env id
                             return t
    (EFuncApp id args) -> do (ft, ts) <- lookupSign env id
                             es <- sequence $ [checkExpr env arg | arg <- args]
                             if length ts /= length es || or [t /= e | (Arg t _, e) <- zip ts es] 
                                then typeError id ts es
                                else return ft

    (EPostInc id)      -> do t <- lookupVar env id
                             if t `elem` [TInt, TDouble] 
                                then return t
                                else typeError id [TInt, TDouble] t
    (EPostDec id)      -> checkExpr env (EPostInc id)
    (EPreInc id)       -> checkExpr env (EPostInc id)
    (EPreDec id)       -> checkExpr env (EPostInc id)

    (EMul e1 e2)       -> do t1 <- checkExpr env e1
                             t2 <- checkExpr env e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return t1
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EDiv e1 e2)       -> checkExpr env (EMul e1 e2)
    (EAdd e1 e2)       -> checkExpr env (EMul e1 e2)
    (ESub e1 e2)       -> checkExpr env (EMul e1 e2)
    
    (ECmpLT e1 e2)     -> do t1 <- checkExpr env e1
                             t2 <- checkExpr env e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return TBool
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (ECmpGT e1 e2)     -> checkExpr env (ECmpLT e1 e2)
    (ECmpLTE e1 e2)    -> checkExpr env (ECmpLT e1 e2)
    (ECmpGTE e1 e2)    -> checkExpr env (ECmpLT e1 e2)

    (ECmpEQ e1 e2)     -> do t1 <- checkExpr env e1
                             t2 <- checkExpr env e2
                             if t2 == t1 
                                then return TBool
                                else typeError e2 [t1] t2
    (ECmpNEQ e1 e2)    -> checkExpr env (ECmpEQ e1 e2)

    (ELAnd e1 e2)      -> do t1 <- checkExpr env e1
                             t2 <- checkExpr env e2
                             if t1 == TBool 
                                then if t2 == t1 
                                     then return TBool
                                     else typeError e2 [t1] t2
                                else typeError e1 [TBool] t1
    (ELOr e1 e2)       -> checkExpr env (ELAnd e1 e2)

    (EAssign id e)     -> do vt <- lookupVar env id
                             et <- checkExpr env e
                             if et == vt 
                                then return et
                                else typeError e [vt] et

checkVarDecl :: Env -> VarDecl -> Err Env

checkVarDecl env vdecl = case vdecl of
    (MultiDecl t ids)  -> foldM (\env' id -> addAny env' id (TType t)) env ids
    
    (AssDecl   t id e) -> do et <- checkExpr env e
                             if t == et
                               then addAny env id (TType t)
                               else typeError e [t] et
                            
                            

typeError e ts t' = fail (printTree e ++ " has type " ++ printTree t'
                    ++ " expected " ++ treeify ts)
                    where treeify []     = "a miracle" -- shouldn't happen
                          treeify [a]    = printTree a
                          treeify [a, b] = printTree a ++ " or " ++ printTree b
                          treeify (a:as) = printTree a ++ ", " ++ printTree as

-}


-}
