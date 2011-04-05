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
  mapM_ checkDefinition defs
  unless (and $ mapM checkReturn defs) fail $ "Missing return"  

-- | Iterate through all function definitions, modifying the environment
--   by adding them and their types to it.
collectDefinitions :: [Definition] -> State Env ()
collectDefinitions defs = mapM_ finder defs
  where
    finder (Definition returns name args _) = 
      addVar name (TFun returns (map (\(Arg typ _) -> typ) args))

-- | Typecheck an entire function definition (including its’ body)
checkDefinition :: Definition -> State Env ()
checkDefinition (Definition typ _ args (Block body)) = withNewScope $ do
  mapM_ (\(Arg typ id) -> addVar id typ) args
  checkStms typ body

checkBlock :: Type -> Block -> State Env ()
checkBlock returns (Block body) = withNewScope $ checkStms returns body

checkStms :: Type -> [Statement] -> State Env ()
checkStms _    []         = return ()
checkStms rett (stm:stms) = do checkStm  rett stm
                               checkStms rett stms

---

checkReturn :: Definition -> State Env Bool
checkReturn (Definition _ _ _ (Block stms)) = checkReturnStms stms
 where
   checkReturnStms []         = return False
   checkReturnStms (stm:stms) = case stm of
       (SBlock (Block stms2))-> r1 <- checkReturnStms stms2 
                                if r1 
                                   then return True
                                   else checkReturnStms stms
       (SReturn e)           -> return True
       (SReturnV)            -> return True
       (SIf e tstm)          -> case e of
                                    (EBool LTrue) -> do r1 <- checkReturnStms [tstm]
                                                        if r1
                                                           then return True
                                                           else checkReturnStms stms
                                    _             -> checkReturnStms stms                     
       (SIfElse e tstm fstm) -> case e of
                                    (EBool LTrue) -> do r1 <- checkReturnStms [tstm]
                                                        if r1
                                                           then return True
                                                           else checkReturnStms stms
                                    (EBool LFalse) -> do r1 <- checkReturnStms [fstm]
                                                        if r1
                                                           then return True
                                                           else checkReturnStms stms
                                    _              -> do r1 <- checkReturnStms [tstm]
                                                         r2 <- checkReturnStms [fstm]
                                                         if r1 && r2
                                                            then return True
                                                            else checkReturnStms stms
       _                     -> checkReturnStms stms

checkStm :: Type -> Statement -> State Env ()
checkStm rett stm = case stm of
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
                                   then checkStm rett tstm
                                   else typeError e [TBool] t 
    (SIfElse e tstm fstm) -> do t <- infer e
                                if t == TBool 
                                   then do checkStm rett tstm
                                           checkStm rett fstm
                                   else typeError e [TBool] t
    (SWhile e stm)        -> do t <- infer e
                                if t == TBool 
                                   then do checkStm rett stm
                                   else typeError e [TBool] t 
    (SExpr e)             -> void (infer e)
    

varDecl :: Type -> Declaration -> State Env ()
varDecl t decl = case decl of
    (DNoInit id) -> addVar id t
    (DInit id e) -> do t' <- infer e
                       if t' /= t
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
-- Utility
--

void :: (Monad m) => m a -> m ()
void = (>> return ())

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

-- | Push an empty scope atop the environment temporarily, restoring the
--   old environment upon completion.
withNewScope :: State Env x -> State Env x
withNewScope code = do
  scopes <- get
  pushScope
  returns <- code
  put scopes
  return returns
