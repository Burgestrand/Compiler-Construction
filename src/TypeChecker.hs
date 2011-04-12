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

typecheck :: Program -> Err ()
typecheck (Program defs) = flip evalStateT emptyEnv $ do
  collectDefinitions defs       -- Fills enviroment with function signatures
  mapM_ checkDefinition defs    -- Typechecks all functions
  mapM_ checkReturn defs        -- Check that all functions return

-- | Iterate through all function definitions, modifying the environment
--   by adding them and their types to it.
collectDefinitions :: [Definition] -> FailStateM ()
collectDefinitions defs = mapM_ finder defs
  where
    finder (Definition returns name args _) = 
      addVar name (TFun returns (map (\(Arg typ _) -> typ) args))

---

-- | Typecheck an entire function definition (including its’ body)
checkDefinition :: Definition -> FailStateM ()
checkDefinition (Definition typ _ args (Block body)) = withNewScope $ do
  mapM_ (\(Arg typ id) -> addVar id typ) args
  checkStatements typ body

-- | Make sure a function always returns
checkReturn :: Definition -> FailStateM ()
checkReturn (Definition TVoid _ _ _)            = return ()
checkReturn (Definition _ name  _ (Block stms)) = do
  returning <- checkReturnStatements stms
  unless returning $ 
    fail $ "Function " ++ show name ++ " cannot safely be assumed to return"
  where
    -- | Checks if the statements always return
    checkReturnStatements :: [Statements] -> State Env Bool
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
checkBlock :: Type -> Block -> FailStateM ()
checkBlock returns (Block body) = withNewScope (checkStatements returns body)

checkStatements :: Type -> [Statement] -> FailStateM ()
checkStatements returns ss = mapM_ (checkStatement returns) ss

checkStatement :: Type -> Statement -> FailStateM ()
checkStatement rett stm = case stm of
    (SEmpty)              -> return ()
    (SBlock b)            -> checkBlock rett b
    (SDeclaration t ds) -> mapM_ (varDecl t) ds
    (SReturn e)           -> do t <- infer e
                                if t /= rett 
                                   then typeError stm [rett] t
                                   else return ()
    (SReturnV)            -> if TVoid /= rett 
                                then typeError stm [rett] TVoid
                                else return ()
    (SIf e tstm)          -> do t <- infer e
                                if t == TBool 
                                   then checkStatement rett tstm
                                   else typeError e [TBool] t 
    (SIfElse e tstm fstm) -> do t <- infer e
                                if t == TBool 
                                   then do checkStatement rett tstm
                                           checkStatement rett fstm
                                   else typeError e [TBool] t
    (SWhile e stm)        -> do t <- infer e
                                if t == TBool 
                                   then do checkStatement rett stm
                                   else typeError e [TBool] t 
    (SInc id)             -> do t <- lookupVar id
                                if t == Tint 
                                   then return ()
                                   else typeError id [TInt] t
    (SDec id)             -> checkStatement rett (SInc id)
    (SAss id e)           -> do t1 <- lookupVar id
                                t2 <- infer e
                                if t1 == t2
                                   then return ()
                                   else typeError id [t1] t2
    (SExpr e)             -> infer e >> return ()
    

varDecl :: Type -> Declaration -> FailStateM ()
varDecl t decl = case decl of
    (DNoInit id) -> addVar id t
    (DInit id e) -> do t' <- infer e
                       if t' /= t
                          then typeError decl [t] t'
                          else addVar id t

                                    
infer :: Expr -> FailStateM Type
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
                             -- TODO Hantera strängar!!
                             case f' of
                                TFun t ts | ts /= ts' -> typeError id [TFun t ts] (TFun t ts')
                                          | otherwise -> return t
                                _ -> fail $ (printTree id ) ++ " isn't a function"
    (ENeg e)           -> do t <- infer e
                             if t `elem` [TInt, TDouble] 
                                then return t
                                else typeError e [TInt, TDouble] t
    (ENot e)           -> do t <- infer e
                             if t `elem` [TBool] 
                                then return t
                                else typeError e [TBool] t
    (EMul e1 op e2)    -> do t1 <- infer e1
                             t2 <- infer e2
                             if t1 `elem` [TInt, TDouble] 
                                then if t2 == t1 
                                     then return t1
                                     else typeError e2 [t1] t2
                                else typeError e1 [TInt, TDouble] t1
    (EAdd e1 op e2)    -> infer (EMul e1 Times e2)
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
    (EOr e1 e2)        -> infer (EAnd e1 e2)
    (EAss id e)        -> do vt <- lookupVar id
                             et <- infer e
                             if et == vt 
                                then return et
                                else typeError e [vt] et
                            
                            
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
