module Env where

import qualified Data.Map as Map

type Scope = Map.Map Identifier Type
type Env   = [Scope]

emptyEnv :: Env
emptyEnv = [Map.empty]

-- adds a identifier to the current scope
addAny :: Env -> Identifier -> Type -> Err Env
addAny (scope:rest) x t | Map.member x scope = fail $
                            "Identifier " ++ printTree x ++ " already declared."
                        | otherwise          = return ((Map.insert x t scope):rest)

lookupAny :: Env -> Identifier -> Err TType
lookupAny []           x = fail $ "Unknown identifier " ++ printTree x ++ "."
lookupAny (scope:rest) x | Map.member x scope = return $ (Map.!) scope x
                         | otherwise          = lookupAny rest x
                         
lookupVar :: Env -> Identifier -> Err Type
lookupVar env x = case lookupAny env x of
                    Ok (TType t) -> return t
                    Ok _         -> fail $ printTree x ++ " is not a variable."
                    Bad e        -> Bad e
                         
lookupSign :: Env -> Identifier -> Err (Type, [Arg])
lookupSign env x = case lookupAny env x of
                     Ok (Sign t args) -> return (t, args)
                     Ok _             -> fail $ printTree x ++ " is not a function."
                     Bad e            -> Bad e

addScope :: Env -> Env
addScope env = Map.empty:env
