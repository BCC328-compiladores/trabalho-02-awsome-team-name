{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Interpreter for the SL language.
--
-- Implements the operational semantics of SL by walking the AST
-- and evaluating expressions/statements in an environment.
-- Supports: functions (including recursion), structs, arrays,
-- control flow (if/else, while, for), and the built-in print function.
module SL.Interpreter
    ( -- * Running programs
      interpret
    , interpretProgram
      -- * Runtime values
    , Value(..)
      -- * Errors
    , RuntimeError(..)
    , prettyRuntimeError
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Control.Monad (forM_, when, void)
import Control.Exception (Exception, throwIO, catch)
import Data.Typeable (Typeable)
import Data.List (intercalate)

import SL.AST

-- ============================================================
-- Runtime values
-- ============================================================

-- | Runtime values in the SL language
data Value
    = VInt Integer           -- ^ Integer value
    | VFloat Double          -- ^ Float value
    | VString Text           -- ^ String value
    | VBool Bool             -- ^ Boolean value
    | VArray (IORef [IORef Value])  -- ^ Mutable array of mutable elements
    | VStruct Text (IORef (Map Text (IORef Value)))  -- ^ Struct: name + mutable fields
    | VFunc FuncDef          -- ^ Function value (for first-class functions)
    | VVoid                  -- ^ Void / unit value

-- | Function definition stored at runtime
data FuncDef = FuncDef
    { fdTypeVars :: [TypeVar]
    , fdName     :: Text
    , fdParams   :: [FuncParam]
    , fdBody     :: [Stmt]
    , fdClosure  :: Env  -- ^ Captured environment (for closures)
    }

instance Show Value where
    show (VInt n)      = show n
    show (VFloat f)    = show f
    show (VString s)   = T.unpack s
    show (VBool True)  = "true"
    show (VBool False) = "false"
    show (VArray _)    = "<array>"
    show (VStruct n _) = "<struct " ++ T.unpack n ++ ">"
    show (VFunc fd)    = "<function " ++ T.unpack (fdName fd) ++ ">"
    show VVoid         = "void"

-- ============================================================
-- Runtime errors
-- ============================================================

-- | Runtime errors
data RuntimeError = RuntimeError
    { rteMessage :: !Text
    } deriving (Show, Typeable)

instance Exception RuntimeError

prettyRuntimeError :: RuntimeError -> String
prettyRuntimeError (RuntimeError msg) = "Runtime error: " ++ T.unpack msg

throwRTE :: Text -> IO a
throwRTE = throwIO . RuntimeError

-- ============================================================
-- Control flow exceptions
-- ============================================================

-- | Used to implement 'return' statements
data ReturnException = ReturnException !Value
    deriving (Show, Typeable)

instance Exception ReturnException

-- ============================================================
-- Environment
-- ============================================================

-- | Runtime environment: stack of scopes (each scope maps names to mutable refs)
data Env = Env
    { envScopes    :: ![Map Text (IORef Value)]   -- ^ Variable scopes (head = innermost)
    , envGlobals   :: !(IORef (Map Text FuncDef)) -- ^ Global function definitions
    , envStructDefs :: !(Map Text StructDefRT)     -- ^ Struct definitions
    }

-- | Struct definition at runtime
data StructDefRT = StructDefRT
    { sdrtName   :: !Text
    , sdrtFields :: ![(Text, Type)]
    }

-- | Create an empty environment
emptyEnv :: IO Env
emptyEnv = do
    globals <- newIORef Map.empty
    return Env
        { envScopes     = [Map.empty]
        , envGlobals    = globals
        , envStructDefs = Map.empty
        }

-- | Look up a variable in the environment (innermost scope first)
lookupVarRT :: Text -> Env -> IO (Maybe (IORef Value))
lookupVarRT name env = go (envScopes env)
  where
    go [] = return Nothing
    go (s:ss) = case Map.lookup name s of
        Just ref -> return (Just ref)
        Nothing  -> go ss

-- | Get a variable's value, throwing an error if not found
getVar :: Text -> Env -> IO Value
getVar name env = do
    mref <- lookupVarRT name env
    case mref of
        Just ref -> readIORef ref
        Nothing  -> throwRTE $ "Undefined variable '" <> name <> "'"

-- | Set a variable's value, throwing an error if not found
setVar :: Text -> Value -> Env -> IO ()
setVar name val env = do
    mref <- lookupVarRT name env
    case mref of
        Just ref -> writeIORef ref val
        Nothing  -> throwRTE $ "Undefined variable '" <> name <> "'"

-- | Define a new variable in the current (innermost) scope
defineVar :: Text -> Value -> Env -> IO Env
defineVar name val env = do
    ref <- newIORef val
    case envScopes env of
        []     -> do
            return env { envScopes = [Map.singleton name ref] }
        (s:ss) -> return env { envScopes = Map.insert name ref s : ss }

-- | Push a new empty scope
pushScopeRT :: Env -> Env
pushScopeRT env = env { envScopes = Map.empty : envScopes env }

-- | Pop the innermost scope
popScopeRT :: Env -> Env
popScopeRT env = case envScopes env of
    []     -> env
    [s]    -> env  -- keep global scope
    (_:ss) -> env { envScopes = ss }

-- | Look up a function definition
lookupFuncRT :: Text -> Env -> IO (Maybe FuncDef)
lookupFuncRT name env = do
    globals <- readIORef (envGlobals env)
    return $ Map.lookup name globals

-- | Register a function definition
registerFunc :: Text -> FuncDef -> Env -> IO ()
registerFunc name fd env =
    modifyIORef' (envGlobals env) (Map.insert name fd)

-- ============================================================
-- Public API
-- ============================================================

-- | Interpret a program and return the exit value (from main's return).
-- Output from print is written to stdout.
interpret :: Program -> IO (Either RuntimeError Value)
interpret prog =
    (Right <$> interpretProgram prog) `catch` (\e -> return (Left (e :: RuntimeError)))

-- | Interpret a program, throwing RuntimeError on failure.
interpretProgram :: Program -> IO Value
interpretProgram (Program tops) = do
    env0 <- emptyEnv
    -- Pass 1: Register all struct definitions
    let env1 = registerStructs tops env0
    -- Pass 2: Register all function definitions
    registerFunctions tops env1
    -- Pass 3: Call main()
    mMain <- lookupFuncRT "main" env1
    case mMain of
        Nothing -> throwRTE "No 'main' function defined"
        Just mainFd -> callFunction mainFd [] env1

-- | Register struct definitions
registerStructs :: [TopLevel] -> Env -> Env
registerStructs [] env = env
registerStructs (TLStruct name fields : rest) env =
    let fieldPairs = map (\(StructField n t) -> (n, t)) fields
        def = StructDefRT name fieldPairs
        env' = env { envStructDefs = Map.insert name def (envStructDefs env) }
    in registerStructs rest env'
registerStructs (_ : rest) env = registerStructs rest env

-- | Register function definitions
registerFunctions :: [TopLevel] -> Env -> IO ()
registerFunctions [] _ = return ()
registerFunctions (TLFunc tvs name params _retType body : rest) env = do
    let fd = FuncDef
            { fdTypeVars = tvs
            , fdName     = name
            , fdParams   = params
            , fdBody     = body
            , fdClosure  = env
            }
    registerFunc name fd env
    registerFunctions rest env
registerFunctions (_ : rest) env = registerFunctions rest env

-- ============================================================
-- Function calls
-- ============================================================

-- | Call a function with given arguments
callFunction :: FuncDef -> [Value] -> Env -> IO Value
callFunction fd args env = do
    -- Create a new scope for function execution
    let funcEnv = pushScopeRT env
    -- Bind parameters to arguments
    funcEnv' <- bindParams (fdParams fd) args funcEnv
    -- Execute function body, catching ReturnException
    execBody (fdBody fd) funcEnv' `catch` handleReturn
  where
    handleReturn :: ReturnException -> IO Value
    handleReturn (ReturnException val) = return val

    execBody :: [Stmt] -> Env -> IO Value
    execBody [] _ = return VVoid
    execBody stmts e = do
        e' <- execStmts stmts e
        return VVoid

-- | Bind parameter names to argument values
bindParams :: [FuncParam] -> [Value] -> Env -> IO Env
bindParams [] _ env = return env
bindParams _ [] env = return env
bindParams (FuncParam name _ : ps) (val : vs) env = do
    env' <- defineVar name val env
    bindParams ps vs env'

-- ============================================================
-- Statement execution
-- ============================================================

-- | Execute a list of statements, threading the environment through
execStmts :: [Stmt] -> Env -> IO Env
execStmts [] env = return env
execStmts (s:ss) env = do
    env' <- execStmt s env
    execStmts ss env'

-- | Execute a single statement
execStmt :: Stmt -> Env -> IO Env

-- Expression statement: evaluate and discard result
execStmt (SExpr expr) env = do
    _ <- evalExpr expr env
    return env

-- Let binding
execStmt (SLet name mty mval) env = do
    val <- case mval of
        Just expr -> evalExpr expr env
        Nothing   -> defaultValue mty env  -- initialize based on type
    defineVar name val env

-- Assignment: lhs = rhs
execStmt (SAssign lhs rhs) env = do
    val <- evalExpr rhs env
    assignTo lhs val env
    return env

-- If-else
execStmt (SIf cond thenB elseB) env = do
    condVal <- evalExpr cond env
    case condVal of
        VBool True -> do
            let env' = pushScopeRT env
            _ <- execStmts thenB env'
            return env
        VBool False -> case elseB of
            Nothing -> return env
            Just stmts -> do
                let env' = pushScopeRT env
                _ <- execStmts stmts env'
                return env
        _ -> throwRTE "if condition must evaluate to a boolean"

-- While loop
execStmt (SWhile cond body) env = do
    let loop = do
            condVal <- evalExpr cond env
            case condVal of
                VBool True -> do
                    let env' = pushScopeRT env
                    _ <- execStmts body env'
                    loop
                VBool False -> return env
                _ -> throwRTE "while condition must evaluate to a boolean"
    loop

-- For loop: for (var = init; cond; update) { body }
execStmt (SFor var initE cond update body) env = do
    let env' = pushScopeRT env
    initVal <- evalExpr initE env'
    env'' <- defineVar var initVal env'
    let loop e = do
            condVal <- evalExpr cond e
            case condVal of
                VBool True -> do
                    let bodyEnv = pushScopeRT e
                    _ <- execStmts body bodyEnv
                    updateVal <- evalExpr update e
                    setVar var updateVal e
                    loop e
                VBool False -> return ()
                _ -> throwRTE "for condition must evaluate to a boolean"
    loop env''
    return env

-- Return
execStmt (SReturn mExpr) env = do
    val <- case mExpr of
        Just expr -> evalExpr expr env
        Nothing   -> return VVoid
    throwIO (ReturnException val)

-- Block
execStmt (SBlock stmts) env = do
    let env' = pushScopeRT env
    _ <- execStmts stmts env'
    return env

-- ============================================================
-- Assignment target resolution
-- ============================================================

-- | Assign a value to an lvalue (variable, array element, or struct field)
assignTo :: Expr -> Value -> Env -> IO ()

-- Simple variable assignment
assignTo (EVar name) val env = setVar name val env

-- Array index assignment: arr[i] = val
assignTo (EIndex arrExpr idxExpr) val env = do
    arrVal <- evalExpr arrExpr env
    idxVal <- evalExpr idxExpr env
    case (arrVal, idxVal) of
        (VArray arrRef, VInt idx) -> do
            elems <- readIORef arrRef
            let i = fromIntegral idx
            if i >= 0 && i < length elems
                then writeIORef (elems !! i) val
                else throwRTE $ "Array index out of bounds: " <> T.pack (show idx)
        (VArray _, _) -> throwRTE "Array index must be an integer"
        _ -> throwRTE "Cannot index non-array value"

-- Field assignment: obj.field = val
assignTo (EField objExpr fieldName) val env = do
    objVal <- evalExpr objExpr env
    case objVal of
        VStruct _ fieldsRef -> do
            fields <- readIORef fieldsRef
            case Map.lookup fieldName fields of
                Just ref -> writeIORef ref val
                Nothing  -> throwRTE $ "Struct has no field '" <> fieldName <> "'"
        _ -> throwRTE "Cannot access field on non-struct value"

assignTo (EFieldAccess objExpr fieldName) val env =
    assignTo (EField objExpr fieldName) val env

assignTo _ _ _ = throwRTE "Invalid assignment target"

-- ============================================================
-- Expression evaluation
-- ============================================================

-- | Evaluate an expression to a runtime value
evalExpr :: Expr -> Env -> IO Value

-- Literals
evalExpr (EInt n) _ = return $ VInt n
evalExpr (EFloat f) _ = return $ VFloat f
evalExpr (EString s) _ = return $ VString s
evalExpr (EBool b) _ = return $ VBool b

-- Variable reference
evalExpr (EVar name) env = getVar name env

-- Binary operation
evalExpr (EBinOp op l r) env = do
    lVal <- evalExpr l env
    rVal <- evalExpr r env
    evalBinOp op lVal rVal

-- Unary operation
evalExpr (EUnaryOp op e) env = do
    val <- evalExpr e env
    evalUnaryOp op val

-- Function call
evalExpr (ECall name args) env = do
    argVals <- mapM (\a -> evalExpr a env) args
    -- Check for built-in functions first
    case name of
        "print" -> do
            builtinPrint argVals
            return VVoid
        _ -> do
            mfd <- lookupFuncRT name env
            case mfd of
                Just fd -> callFunction fd argVals env
                Nothing -> do
                    -- Check if it's a variable holding a function
                    mref <- lookupVarRT name env
                    case mref of
                        Just ref -> do
                            val <- readIORef ref
                            case val of
                                VFunc fd -> callFunction fd argVals env
                                _ -> throwRTE $ "'" <> name <> "' is not a function"
                        Nothing -> throwRTE $ "Undefined function '" <> name <> "'"

-- Array indexing: arr[i]
evalExpr (EIndex arrExpr idxExpr) env = do
    arrVal <- evalExpr arrExpr env
    idxVal <- evalExpr idxExpr env
    case (arrVal, idxVal) of
        (VArray arrRef, VInt idx) -> do
            elems <- readIORef arrRef
            let i = fromIntegral idx
            if i >= 0 && i < length elems
                then readIORef (elems !! i)
                else throwRTE $ "Array index out of bounds: " <> T.pack (show idx)
                            <> " (size: " <> T.pack (show (length elems)) <> ")"
        (VArray _, _) -> throwRTE "Array index must be an integer"
        _ -> throwRTE "Cannot index non-array value"

-- Field access: e.field
evalExpr (EField e fieldName) env = evalFieldAccess e fieldName env

-- Field access (alternative node)
evalExpr (EFieldAccess e fieldName) env = evalFieldAccess e fieldName env

-- Array literal: [1, 2, 3]
evalExpr (EArray elems) env = do
    vals <- mapM (\e -> evalExpr e env) elems
    refs <- mapM newIORef vals
    arrRef <- newIORef refs
    return $ VArray arrRef

-- New: new Type[size]
evalExpr (ENew _ty sizeExpr) env = do
    sizeVal <- evalExpr sizeExpr env
    case sizeVal of
        VInt n -> do
            let size = fromIntegral n
            refs <- mapM (\_ -> newIORef VVoid) [1..size]
            arrRef <- newIORef refs
            return $ VArray arrRef
        _ -> throwRTE "Array size must be an integer"

-- Struct literal: Person{"Alice", 25}
evalExpr (EStruct name argExprs) env = do
    case Map.lookup name (envStructDefs env) of
        Nothing -> throwRTE $ "Undefined struct '" <> name <> "'"
        Just def -> do
            let fieldNames = map fst (sdrtFields def)
            argVals <- mapM (\e -> evalExpr e env) argExprs
            if length fieldNames /= length argVals
                then throwRTE $ "Struct '" <> name <> "' expects "
                    <> T.pack (show (length fieldNames)) <> " fields but got "
                    <> T.pack (show (length argVals))
                else do
                    pairs <- mapM (\(fn, val) -> do
                        ref <- newIORef val
                        return (fn, ref)) (zip fieldNames argVals)
                    fieldsRef <- newIORef (Map.fromList pairs)
                    return $ VStruct name fieldsRef

-- ============================================================
-- Field access helper
-- ============================================================

evalFieldAccess :: Expr -> Text -> Env -> IO Value
evalFieldAccess e fieldName env = do
    val <- evalExpr e env
    case fieldName of
        -- .size is a built-in property for arrays
        "size" -> case val of
            VArray arrRef -> do
                elems <- readIORef arrRef
                return $ VInt (fromIntegral (length elems))
            _ -> accessStructField val fieldName
        _ -> accessStructField val fieldName

accessStructField :: Value -> Text -> IO Value
accessStructField (VStruct _ fieldsRef) fieldName = do
    fields <- readIORef fieldsRef
    case Map.lookup fieldName fields of
        Just ref -> readIORef ref
        Nothing  -> throwRTE $ "Struct has no field '" <> fieldName <> "'"
accessStructField val fieldName =
    throwRTE $ "Cannot access field '" <> fieldName <> "' on " <> T.pack (show val)

-- ============================================================
-- Binary operations
-- ============================================================

evalBinOp :: BinOp -> Value -> Value -> IO Value

-- Arithmetic
evalBinOp OpAdd (VInt a)   (VInt b)   = return $ VInt (a + b)
evalBinOp OpAdd (VFloat a) (VFloat b) = return $ VFloat (a + b)
evalBinOp OpAdd (VInt a)   (VFloat b) = return $ VFloat (fromIntegral a + b)
evalBinOp OpAdd (VFloat a) (VInt b)   = return $ VFloat (a + fromIntegral b)
evalBinOp OpAdd (VString a) (VString b) = return $ VString (a <> b)  -- string concatenation
evalBinOp OpAdd l r = throwRTE $ "Cannot add " <> showValType l <> " and " <> showValType r

evalBinOp OpSub (VInt a)   (VInt b)   = return $ VInt (a - b)
evalBinOp OpSub (VFloat a) (VFloat b) = return $ VFloat (a - b)
evalBinOp OpSub (VInt a)   (VFloat b) = return $ VFloat (fromIntegral a - b)
evalBinOp OpSub (VFloat a) (VInt b)   = return $ VFloat (a - fromIntegral b)
evalBinOp OpSub l r = throwRTE $ "Cannot subtract " <> showValType l <> " and " <> showValType r

evalBinOp OpMul (VInt a)   (VInt b)   = return $ VInt (a * b)
evalBinOp OpMul (VFloat a) (VFloat b) = return $ VFloat (a * b)
evalBinOp OpMul (VInt a)   (VFloat b) = return $ VFloat (fromIntegral a * b)
evalBinOp OpMul (VFloat a) (VInt b)   = return $ VFloat (a * fromIntegral b)
evalBinOp OpMul l r = throwRTE $ "Cannot multiply " <> showValType l <> " and " <> showValType r

evalBinOp OpDiv (VInt a)   (VInt b)
    | b == 0    = throwRTE "Division by zero"
    | otherwise = return $ VInt (a `div` b)
evalBinOp OpDiv (VFloat a) (VFloat b)
    | b == 0    = throwRTE "Division by zero"
    | otherwise = return $ VFloat (a / b)
evalBinOp OpDiv (VInt a)   (VFloat b)
    | b == 0    = throwRTE "Division by zero"
    | otherwise = return $ VFloat (fromIntegral a / b)
evalBinOp OpDiv (VFloat a) (VInt b)
    | b == 0    = throwRTE "Division by zero"
    | otherwise = return $ VFloat (a / fromIntegral b)
evalBinOp OpDiv l r = throwRTE $ "Cannot divide " <> showValType l <> " and " <> showValType r

evalBinOp OpMod (VInt a) (VInt b)
    | b == 0    = throwRTE "Modulo by zero"
    | otherwise = return $ VInt (a `mod` b)
evalBinOp OpMod l r = throwRTE $ "Cannot modulo " <> showValType l <> " and " <> showValType r

-- Comparison
evalBinOp OpLt  l r = compareValues (<)  l r
evalBinOp OpLe  l r = compareValues (<=) l r
evalBinOp OpGt  l r = compareValues (>)  l r
evalBinOp OpGe  l r = compareValues (>=) l r

-- Equality
evalBinOp OpEq  l r = equalityCheck (==) l r
evalBinOp OpNeq l r = equalityCheck (/=) l r

-- Logical
evalBinOp OpAnd (VBool a) (VBool b) = return $ VBool (a && b)
evalBinOp OpAnd l r = throwRTE $ "Cannot apply && to " <> showValType l <> " and " <> showValType r

evalBinOp OpOr (VBool a) (VBool b) = return $ VBool (a || b)
evalBinOp OpOr l r = throwRTE $ "Cannot apply || to " <> showValType l <> " and " <> showValType r

-- | Compare two numeric values
compareValues :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> IO Value
compareValues cmp (VInt a)   (VInt b)   = return $ VBool (cmp a b)
compareValues cmp (VFloat a) (VFloat b) = return $ VBool (cmp a b)
compareValues cmp (VInt a)   (VFloat b) = return $ VBool (cmp (fromIntegral a) b)
compareValues cmp (VFloat a) (VInt b)   = return $ VBool (cmp a (fromIntegral b))
compareValues _ l r = throwRTE $ "Cannot compare " <> showValType l <> " and " <> showValType r

-- | Check equality between two values
equalityCheck :: (forall a. Eq a => a -> a -> Bool) -> Value -> Value -> IO Value
equalityCheck cmp (VInt a)    (VInt b)    = return $ VBool (cmp a b)
equalityCheck cmp (VFloat a)  (VFloat b)  = return $ VBool (cmp a b)
equalityCheck cmp (VInt a)    (VFloat b)  = return $ VBool (cmp (fromIntegral a) b)
equalityCheck cmp (VFloat a)  (VInt b)    = return $ VBool (cmp a (fromIntegral b))
equalityCheck cmp (VString a) (VString b) = return $ VBool (cmp a b)
equalityCheck cmp (VBool a)   (VBool b)   = return $ VBool (cmp a b)
equalityCheck _ l r = throwRTE $ "Cannot compare " <> showValType l <> " and " <> showValType r

-- ============================================================
-- Unary operations
-- ============================================================

evalUnaryOp :: UnaryOp -> Value -> IO Value
evalUnaryOp OpNeg (VInt n)   = return $ VInt (-n)
evalUnaryOp OpNeg (VFloat f) = return $ VFloat (-f)
evalUnaryOp OpNeg v = throwRTE $ "Cannot negate " <> showValType v

evalUnaryOp OpNot (VBool b)  = return $ VBool (not b)
evalUnaryOp OpNot v = throwRTE $ "Cannot apply ! to " <> showValType v

-- ============================================================
-- Built-in functions
-- ============================================================

-- | Built-in print function: prints any value to stdout
builtinPrint :: [Value] -> IO ()
builtinPrint args = mapM_ printValue args
  where
    printValue :: Value -> IO ()
    printValue (VInt n)    = putStrLn (show n)
    printValue (VFloat f)  = putStrLn (show f)
    printValue (VString s) = putStrLn (T.unpack s)
    printValue (VBool b)   = putStrLn (if b then "true" else "false")
    printValue VVoid       = putStrLn "void"
    printValue v           = putStrLn (show v)

-- ============================================================
-- Helpers
-- ============================================================

-- | Create a default value based on a type annotation.
-- Used for declarations like 'let people : Person[3];' without an initializer.
defaultValue :: Maybe Type -> Env -> IO Value
defaultValue Nothing _ = return VVoid
defaultValue (Just ty) env = defaultForType ty env

defaultForType :: Type -> Env -> IO Value
defaultForType TyInt _ = return $ VInt 0
defaultForType TyFloat _ = return $ VFloat 0.0
defaultForType TyString _ = return $ VString ""
defaultForType TyBool _ = return $ VBool False
defaultForType TyVoid _ = return VVoid
defaultForType (TyArray elemTy (Just sizeExpr)) env = do
    sizeVal <- evalExpr sizeExpr env
    case sizeVal of
        VInt n -> do
            let size = fromIntegral n
            refs <- mapM (\_ -> do
                v <- defaultForType elemTy env
                newIORef v) [1..size :: Integer]
            arrRef <- newIORef refs
            return $ VArray arrRef
        _ -> do
            arrRef <- newIORef []
            return $ VArray arrRef
defaultForType (TyArray _ Nothing) _ = do
    arrRef <- newIORef []
    return $ VArray arrRef
defaultForType (TyVar _) _ = return VVoid  -- type variable, can't determine default
defaultForType (TyStruct _) _ = return VVoid
defaultForType (TyFunc _ _) _ = return VVoid

-- | Show the type name of a runtime value
showValType :: Value -> Text
showValType (VInt _)      = "int"
showValType (VFloat _)    = "float"
showValType (VString _)   = "string"
showValType (VBool _)     = "bool"
showValType (VArray _)    = "array"
showValType (VStruct n _) = n
showValType (VFunc _)     = "function"
showValType VVoid         = "void"
