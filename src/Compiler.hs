data CompileState = CompileState {Env :: env, stacksize :: Integer, maxstack :: Integer, nextlabel :: Integer, code :: String}

type Compiling = State CompileState

compileProgram :: Program -> String

compileDefinition :: Env -> Definition -> String

compileBlock :: Compiling String

compileStatement :: Compiling String

compileExpr :: Compiling String

-- Help functions

getLabel    :: Compiling Label
goto        :: Label -> Compiling String
label       :: Label -> Compiling String
pushInt     :: Integer -> Compiling String
pushDouble  :: Double -> Compiling String
pushString  :: String -> Compiling String
pushBool    :: Bool -> Compiling String
pop         :: Compiling String


t1 <- complie e
emit ".stacksize" getStackSize
emit t1

type CompileM = State { stacksize :: Integer, output :: String }

compile $ do
  l <- getLabel
  push "Hello world"
  push "Yes"
  invoke "java.lang.String.concat"
  pop
  return

compile :: CompileM -> String
compile m = do
  result <- run m
  stacksize ++ output
  
  
program :: Program -> name -> Code
definintion :: (Env, Definition) -> Code
Statement :: (Env, Statement) -> Stacksize -> MaxStack -> labelgen -> (Stacksize, MaxStack, labelgen, Code)
Expr :: (Env, Expr) -> Stacksize -> MaxStack -> (Stacksize, MaxStack, Code)





