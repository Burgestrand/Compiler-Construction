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
type FailStateM = StateT Env Err

--
-- Typechecker
--

typecheck :: Program -> Err Program
typecheck (Program defs) = flip evalStateT emptyEnv $ do
  collectDefinitions defs       -- Fills enviroment with function signatures
  
  -- Add built-in functions! :D
  addVar (Ident "printInt") (TFun TVoid [TInt])
  addVar (Ident "printDouble") (TFun TVoid [TDouble])
  addVar (Ident "printString") (TFun TVoid [TVoid])
  
  addVar (Ident "readInt") (TFun TInt [])
  addVar (Ident "readDouble") (TFun TDouble [])
  
  mapM_ checkReturn defs        -- Check that all functions return
  defs <- mapM checkDefinition defs    -- Typechecks all functions
  return (Program defs)

-- | Iterate through all function definitions, modifying the environment
--   by adding them and their types to it.
collectDefinitions :: [Definition] -> FailStateM ()
collectDefinitions defs = mapM_ finder defs
  where
    finder (Definition returns name args _) = 
      addVar name (TFun returns (map (\(Arg typ _) -> typ) args))

-- | Typecheck an entire function definition (including its’ body)
checkDefinition :: Definition -> FailStateM Definition
checkDefinition (Definition typ x args (Block body)) = withNewScope $ do
  mapM_ (\(Arg typ id) -> addVar id typ) args
  body <- checkStatements typ body
  return (Definition typ x args (Block body))

-- | Make sure a function always returns
checkReturn :: Definition -> FailStateM ()
checkReturn (Definition TVoid _ _ _)            = return ()
checkReturn (Definition _ name  _ (Block stms)) = do
  returning <- checkReturnStatements stms
  unless returning $ 
    fail $ "Function " ++ show name ++ " cannot safely be assumed to return"
  where
    -- | Checks if the statements always return
    checkReturnStatements :: [Statement] -> FailStateM Bool
    checkReturnStatements []         = return False
    checkReturnStatements (stm:stms) = case stm of
        (SBlock (Block stms2))-> do r1 <- checkReturnStatements stms2 
                                    if r1 
                                       then return True
                                       else checkReturnStatements stms
        (SReturn e)           -> return True
        (SReturnV)            -> return True
        (SIf e tstm)          -> case e of
                                     (EBool LTrue) -> do r1 <- checkReturnStatements [tstm]
                                                         if r1
                                                            then return True
                                                            else checkReturnStatements stms
                                     _             -> checkReturnStatements stms                     
        (SIfElse e tstm fstm) -> case e of
                                     (EBool LTrue) -> do r1 <- checkReturnStatements [tstm]
                                                         if r1
                                                            then return True
                                                            else checkReturnStatements stms
                                     (EBool LFalse) -> do r1 <- checkReturnStatements [fstm]
                                                          if r1
                                                             then return True
                                                             else checkReturnStatements stms
                                     _              -> do r1 <- checkReturnStatements [tstm]
                                                          r2 <- checkReturnStatements [fstm]
                                                          if r1 && r2
                                                             then return True
                                                             else checkReturnStatements stms
        _                     -> checkReturnStatements stms


-- 
checkBlock :: Type -> Block -> FailStateM Block
checkBlock returns (Block body) = withNewScope $ do
  body <- checkStatements returns body
  return (Block body)

checkStatements :: Type -> [Statement] -> FailStateM [Statement]
checkStatements returns ss = mapM (checkStatement returns) ss

checkStatement :: Type -> Statement -> FailStateM Statement
checkStatement rett stm = case stm of
    (SEmpty)              -> return (SEmpty)
    (SBlock b)            -> do b <- checkBlock rett b
                                return (SBlock b) 
    (SDeclaration t ds)   -> do ds <- mapM (varDecl t) ds
                                return (SDeclaration t ds)
    (SReturn e)           -> do e@(ETyped t _) <- infer e
                                if t /= rett 
                                   then typeError stm [rett] t
                                   else return (SReturn e)
    (SReturnV)            -> if TVoid /= rett 
                                then typeError stm [rett] TVoid
                                else return (SReturnV)
    (SIf e tstm)          -> do e@(ETyped t _) <- infer e
                                if t == TBool 
                                   then do tstm <- checkStatement rett tstm
                                           return (SIf e tstm)
                                   else typeError e [TBool] t 
    (SIfElse e tstm fstm) -> do e@(ETyped t _) <- infer e
                                if t == TBool 
                                   then do tstm <- checkStatement rett tstm
                                           fstm <- checkStatement rett fstm
                                           return (SIfElse e tstm fstm)
                                   else typeError e [TBool] t
    (SWhile e stm)        -> do e@(ETyped t _) <- infer e
                                if t == TBool 
                                   then do stm <- checkStatement rett stm
                                           return (SWhile e stm)
                                   else typeError e [TBool] t 
    (SInc id)             -> do t <- lookupVar id
                                if t == TInt 
                                   then return (SInc id)
                                   else typeError id [TInt] t
    (SDec id)             -> do t <- lookupVar id
                                if t == TInt 
                                   then return (SDec id)
                                   else typeError id [TInt] t
    (SAss id e)           -> do t1 <- lookupVar id
                                e@(ETyped t2 _) <- infer e
                                if t1 == t2
                                   then return (SAss id e)
                                   else typeError id [t1] t2
    (SExpr e)             -> do e@(ETyped _ _) <- infer e
                                return (SExpr e)
    

varDecl :: Type -> Declaration -> FailStateM Declaration
varDecl t decl = case decl of
    (DNoInit id) -> do addVar id t
                       return (DNoInit id)
    (DInit id e) -> do e@(ETyped t' _) <- infer e
                       if t' /= t
                          then typeError decl [t] t'
                          else do addVar id t
                                  return (DInit id e)

                                    
infer :: Expr -> FailStateM Expr
infer e = case e of  
    (EVar id)          -> do t <- lookupVar id
                             return (ETyped t e)
    (EInt _)           -> return (ETyped TInt  e)   
    (EDouble _)        -> return (ETyped TDouble  e)
    (EBool _)          -> return (ETyped TBool e)
    (ECall id args) | id == (Ident "printString") -> case args of
                                                       [EString _] -> return (ETyped TVoid e)
                                                       x           -> fail $ "printString expected a string, got " ++ printTree x
                    | otherwise -> do f' <- lookupVar id
                                      args' <- mapM infer args
                                      let (args, ts') = unzip $ [(arg, t') | arg@(ETyped t' _) <- args']
                                      case f' of
                                        TFun t ts | ts /= ts' -> typeError id [TFun t ts] (TFun t ts')
                                                  | otherwise -> return (ETyped t (ECall id args))
                                        _ -> fail $ (printTree id ) ++ " isn't a function"
    (ENeg e)           -> do e@(ETyped t _) <- infer e
                             if t `elem` [TInt, TDouble] 
                                then return (ETyped t (ENeg e))
                                else typeError e [TInt, TDouble] t
    (ENot e)           -> do e@(ETyped t _) <- infer e
                             if t `elem` [TBool] 
                                then return (ETyped t (ENot e))
                                else typeError e [TBool] t
    (EMul e1 op e2)    -> do e1@(ETyped t1 _) <- infer e1
                             e2@(ETyped t2 _) <- infer e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return (ETyped t1 (EMul e1 op e2))
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EAdd e1 op e2)    -> do e1@(ETyped t1 _) <- infer e1
                             e2@(ETyped t2 _) <- infer e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return (ETyped t1 (EAdd e1 op e2))
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EEqu e1 op e2)    -> do e1@(ETyped t1 _) <- infer e1
                             e2@(ETyped t2 _) <- infer e2
                             if t2 == t1 
                               then return (ETyped TBool (EEqu e1 op e2))
                               else typeError e2 [t1] t2
    (ERel e1 op e2)    -> do e1@(ETyped t1 _) <- infer e1
                             e2@(ETyped t2 _) <- infer e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return (ETyped TBool (ERel e1 op e2))
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EAnd e1 e2)       -> do e1@(ETyped t1 _) <- infer e1
                             e2@(ETyped t2 _) <- infer e2
                             if t1 == TBool 
                                then if t2 == t1 
                                     then return (ETyped TBool (EAnd e1 e2))
                                     else typeError e2 [t1] t2
                                else typeError e1 [TBool] t1
    (EOr e1 e2)        -> do e1@(ETyped t1 _) <- infer e1
                             e2@(ETyped t2 _) <- infer e2
                             if t1 == TBool 
                                then if t2 == t1 
                                     then return (ETyped TBool (EOr e1 e2))
                                     else typeError e2 [t1] t2
                                else typeError e1 [TBool] t1
    (EString s)        -> fail $ "You can't have a string " ++ s ++ " here!"
                            
                            
typeError :: (Monad m, Print a) => a -> [Type] -> Type -> m x
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
addVar :: Ident -> Type -> FailStateM ()
addVar id t = do
  (scope:scopes) <- get
  if Map.member id scope
    then fail $ "Identifier " ++ printTree id ++ " already declared."
    else put $ (Map.insert id t scope):scopes

-- | Find an identifier in the environment
lookupVar :: Ident -> FailStateM Type
lookupVar id = do
  env <- get
  case find (Map.member id) env of
    Just scope -> return $ (Map.!) scope id
    Nothing    -> fail   $ "Unknown identifier " ++ printTree id

-- | Add an empty scope layer atop the environment
pushScope :: FailStateM ()
pushScope = modify (emptyEnv ++)

-- | Push an empty scope atop the environment temporarily, restoring the
--   old environment upon completion.
withNewScope :: FailStateM x -> FailStateM x
withNewScope code = do
  scopes <- get
  pushScope
  returns <- code
  put scopes
  return returns
