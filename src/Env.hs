module Env where

import qualified Data.Map as Map

type Scope = Map.Map Identifier Type
type Env   = [Scope]

-- creates a empty enviroment
emptyEnv :: Env
emptyEnv = [Map.empty]

-- adds a identifier to the current scope
add :: Env -> Identifier -> Type -> Err Env
add (scope:rest) x t | Map.member x scope = fail $ "Identifier " ++ printTree x ++ " already declared."
                     | otherwise          = return ((Map.insert x t scope):rest)

-- finds a variable in the enviroment
lookup :: Env -> Identifier -> Err Type
lookup []           x = fail $ "Unknown identifier " ++ printTree x ++ "."
lookup (scope:rest) x | Map.member x scope = return $ (Map.!) scope x
                      | otherwise          = lookup rest x

-- adds a scope layer
addScope :: Env -> Env
addScope env = Map.empty:env
