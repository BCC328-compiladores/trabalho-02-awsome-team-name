{-# LANGUAGE OverloadedStrings #-}

-- | Type checker and semantic analyzer for the SL language.
--
-- Performs:
--   * Name resolution (variables, functions, structs)
--   * Type checking (binary/unary ops, assignments, returns, conditions)
--   * Basic type inference (parameters/returns without annotations)
--   * Generic (forall) instantiation
--
-- Errors are accumulated so the user sees all problems at once.
module SL.TypeChecker
    ( -- * Main entry point
      typeCheck
    , typeCheckProgram
      -- * Result type
    , TCResult(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.List (foldl')

import SL.AST
import SL.Env

-- | The result of type-checking a program.
data TCResult = TCResult
    { tcErrors   :: ![SemanticError]          -- ^ Accumulated semantic errors
    , tcFuncTypes :: !(Map Text (Type, [Type])) -- ^ Inferred function types: name -> (retType, paramTypes)
    } deriving (Show)

-- ============================================================
-- Internal state threaded through the checker
-- ============================================================

data TCState = TCState
    { tcEnv       :: !Env
    , tcErrs      :: ![SemanticError]
    , tcCurrentFunc :: !(Maybe Text)         -- ^ Name of function being checked
    , tcExpectedRet :: !(Maybe Type)         -- ^ Expected return type of current function
    , tcInferredTypes :: !(Map Text (Type, [Type])) -- ^ Accumulated inferred function signatures
    }

initState :: TCState
initState = TCState
    { tcEnv          = emptyEnv
    , tcErrs         = []
    , tcCurrentFunc  = Nothing
    , tcExpectedRet  = Nothing
    , tcInferredTypes = Map.empty
    }

-- | Add an error to the state
addError :: Int -> Int -> Text -> TCState -> TCState
addError line col msg st =
    st { tcErrs = tcErrs st ++ [SemanticError line col msg] }

addError' :: Text -> TCState -> TCState
addError' = addError 0 0

modifyEnv :: (Env -> Env) -> TCState -> TCState
modifyEnv f st = st { tcEnv = f (tcEnv st) }

-- ============================================================
-- Public API
-- ============================================================

-- | Type-check a program (convenience: takes the raw AST).
typeCheck :: Program -> TCResult
typeCheck = typeCheckProgram

-- | Type-check a full SL program.
typeCheckProgram :: Program -> TCResult
typeCheckProgram (Program tops) =
    let st0  = initState
        -- Pass 1: register all structs and function signatures
        st1  = registerTopLevels tops st0
        -- Pass 2: check function bodies
        st2  = foldl' (flip checkTopLevel) st1 tops
    in TCResult
        { tcErrors    = tcErrs st2
        , tcFuncTypes = tcInferredTypes st2
        }

-- ============================================================
-- Pass 1: Register declarations
-- ============================================================

registerTopLevels :: [TopLevel] -> TCState -> TCState
registerTopLevels tops st = foldl' (flip registerTopLevel) st tops

registerTopLevel :: TopLevel -> TCState -> TCState
registerTopLevel (TLStruct name fields) st =
    let fieldPairs = map (\(StructField n t) -> (n, t)) fields
        def = StructDef name fieldPairs
    in case lookupStruct name (tcEnv st) of
        Just _ -> addError' ("Struct '" <> name <> "' already defined") st
        Nothing -> modifyEnv (insertStruct name def) st

registerTopLevel (TLFunc tvs name params retType _body) st =
    let sig = FuncSig
            { fsTypeVars   = tvs
            , fsParamTypes = map paramType params
            , fsRetType    = retType
            }
    in case lookupFunc name (tcEnv st) of
        Just _ -> addError' ("Function '" <> name <> "' already defined") st
        Nothing -> modifyEnv (insertFunc name sig) st

-- ============================================================
-- Pass 2: Check top-level definitions
-- ============================================================

checkTopLevel :: TopLevel -> TCState -> TCState
checkTopLevel (TLStruct _ _) st = st  -- structs already registered, nothing else to check
checkTopLevel (TLFunc tvs name params retType body) st =
    let -- Push a new scope for the function body
        st1 = modifyEnv pushScope st
        -- Bind type variables as TyVar if we have forall
        st2 = foldl' (\s tv -> modifyEnv (insertVar tv (TyVar tv)) s) st1 tvs
        -- Bind parameters
        st3 = foldl' (flip (bindParam name)) st2 params
        -- Set the current function context
        st4 = st3 { tcCurrentFunc = Just name, tcExpectedRet = retType }
        -- Check body statements
        st5 = checkStmts body st4
        -- Infer return type if not annotated
        st6 = recordFuncType name params retType st5
        -- Pop scope
        st7 = modifyEnv popScope st6
        st8 = st7 { tcCurrentFunc = Nothing, tcExpectedRet = Nothing }
    in st8

-- | Bind a function parameter into the environment.
-- If the parameter has no type annotation, assign a fresh type variable.
bindParam :: Text -> FuncParam -> TCState -> TCState
bindParam _funcName (FuncParam pname mty) st =
    case mty of
        Just ty -> modifyEnv (insertVar pname ty) st
        Nothing ->
            let (tv, env') = freshTyVar (tcEnv st)
            in st { tcEnv = insertVar pname tv env' }

-- | Record inferred function type after checking body
recordFuncType :: Text -> [FuncParam] -> Maybe Type -> TCState -> TCState
recordFuncType name params mRetType st =
    let env = tcEnv st
        paramTypes = map (\(FuncParam pn _) ->
            fromMaybe TyVoid (lookupVar pn env)) params
        retTy = fromMaybe TyVoid mRetType
    in st { tcInferredTypes = Map.insert name (retTy, paramTypes) (tcInferredTypes st) }

-- ============================================================
-- Statement checking
-- ============================================================

checkStmts :: [Stmt] -> TCState -> TCState
checkStmts stmts st = foldl' (flip checkStmt) st stmts

checkStmt :: Stmt -> TCState -> TCState

-- Expression statement
checkStmt (SExpr expr) st =
    let (_ty, st') = inferExpr expr st
    in st'

-- Let binding
checkStmt (SLet name mty mval) st =
    -- Check for duplicate declaration in the current scope
    let st0 = case lookupVarCurrentScope name (tcEnv st) of
            Just _  -> addError' ("Variable '" <> name <> "' already declared in this scope") st
            Nothing -> st
    in case (mty, mval) of
        -- let x : T = e;  → check e : T
        (Just ty, Just val) ->
            let (valTy, st1) = inferExpr val st0
                st2 = checkAssignable ty valTy ("let '" <> name <> "'") st1
            in modifyEnv (insertVar name ty) st2
        -- let x : T;
        (Just ty, Nothing) ->
            modifyEnv (insertVar name ty) st0
        -- let x = e;  → infer type from e
        (Nothing, Just val) ->
            let (valTy, st1) = inferExpr val st0
            in modifyEnv (insertVar name valTy) st1
        -- let x;  → no type, no value (assign a fresh type variable)
        (Nothing, Nothing) ->
            let (tv, env') = freshTyVar (tcEnv st0)
            in st0 { tcEnv = insertVar name tv env' }

-- Assignment: lhs = rhs
checkStmt (SAssign lhs rhs) st =
    let (lhsTy, st1) = inferExpr lhs st
        (rhsTy, st2) = inferExpr rhs st1
        st3 = checkAssignable lhsTy rhsTy "assignment" st2
    in st3

-- If-else
checkStmt (SIf cond thenB elseB) st =
    let (condTy, st1) = inferExpr cond st
        st2 = checkExpectedType TyBool condTy "if condition must be bool" st1
        st3 = modifyEnv pushScope st2
        st4 = checkStmts thenB st3
        st5 = modifyEnv popScope st4
    in case elseB of
        Nothing -> st5
        Just stmts ->
            let st6 = modifyEnv pushScope st5
                st7 = checkStmts stmts st6
            in modifyEnv popScope st7

-- While
checkStmt (SWhile cond body) st =
    let (condTy, st1) = inferExpr cond st
        st2 = checkExpectedType TyBool condTy "while condition must be bool" st1
        st3 = modifyEnv pushScope st2
        st4 = checkStmts body st3
    in modifyEnv popScope st4

-- For
checkStmt (SFor var initE cond update body) st =
    let st0 = modifyEnv pushScope st
        (initTy, st1) = inferExpr initE st0
        st2 = modifyEnv (insertVar var initTy) st1
        (condTy, st3) = inferExpr cond st2
        st4 = checkExpectedType TyBool condTy "for condition must be bool" st3
        (_updateTy, st5) = inferExpr update st4
        st6 = checkStmts body st5
    in modifyEnv popScope st6

-- Return
checkStmt (SReturn mExpr) st =
    case mExpr of
        Nothing ->
            case tcExpectedRet st of
                Just TyVoid -> st
                Just ty     -> addError' ("Expected return value of type " <> showType ty <> " but got empty return") st
                Nothing     -> st  -- inferred void
        Just expr ->
            let (exprTy, st1) = inferExpr expr st
            in case tcExpectedRet st1 of
                Just expectedTy -> checkAssignable expectedTy exprTy "return statement" st1
                Nothing         -> st1  -- no annotation, accept inferred type

-- Block
checkStmt (SBlock stmts) st =
    let st1 = modifyEnv pushScope st
        st2 = checkStmts stmts st1
    in modifyEnv popScope st2

-- ============================================================
-- Expression type inference
-- ============================================================

-- | Infer the type of an expression, returning (type, updatedState).
inferExpr :: Expr -> TCState -> (Type, TCState)

-- Literals
inferExpr (EInt _) st    = (TyInt, st)
inferExpr (EFloat _) st  = (TyFloat, st)
inferExpr (EString _) st = (TyString, st)
inferExpr (EBool _) st   = (TyBool, st)

-- Variable reference
inferExpr (EVar name) st =
    case lookupVar name (tcEnv st) of
        Just ty -> (ty, st)
        Nothing -> (TyVoid, addError' ("Undefined variable '" <> name <> "'") st)

-- Binary operation
inferExpr (EBinOp op l r) st =
    let (lTy, st1) = inferExpr l st
        (rTy, st2) = inferExpr r st1
    in inferBinOp op lTy rTy st2

-- Unary operation
inferExpr (EUnaryOp op e) st =
    let (eTy, st1) = inferExpr e st
    in inferUnaryOp op eTy st1

-- Function call
inferExpr (ECall name args) st =
    let -- Special case: print is a builtin
        st0 = st
    in if name == "print"
        then let st1 = foldl' (\s a -> snd (inferExpr a s)) st0 args
             in (TyVoid, st1)
        else case lookupFunc name (tcEnv st0) of
            Just sig ->
                let (argTypes, st1) = inferExprs args st0
                    expectedLen = length (fsParamTypes sig)
                    actualLen   = length args
                    st2 = if expectedLen /= actualLen
                          then addError' ("Function '" <> name <> "' expects "
                                         <> T.pack (show expectedLen) <> " arguments but got "
                                         <> T.pack (show actualLen)) st1
                          else checkArgTypes name (fsParamTypes sig) argTypes st1
                    retTy = fromMaybe TyVoid (fsRetType sig)
                in (retTy, st2)
            Nothing ->
                -- Check if it's a variable holding a function type (e.g. higher-order parameter)
                case lookupVar name (tcEnv st0) of
                    Just (TyFunc paramTys retTy) ->
                        let (argTypes, st1) = inferExprs args st0
                            expectedLen = length paramTys
                            actualLen   = length args
                            st2 = if expectedLen /= actualLen
                                  then addError' ("Function '" <> name <> "' expects "
                                                 <> T.pack (show expectedLen) <> " arguments but got "
                                                 <> T.pack (show actualLen)) st1
                                  else foldl' (\s (expected, actual) ->
                                      checkAssignable expected actual ("argument of '" <> name <> "'") s)
                                      st1 (zip paramTys argTypes)
                        in (retTy, st2)
                    Just (TyVar _) ->
                        -- Type variable (generic), accept any call
                        let (_, st1) = inferExprs args st0
                        in (TyVar "_call_result", st1)
                    _ ->
                        let st1 = foldl' (\s a -> snd (inferExpr a s)) st0 args
                        in (TyVoid, addError' ("Undefined function '" <> name <> "'") st1)

-- Array indexing: arr[i]
inferExpr (EIndex arr idx) st =
    let (arrTy, st1) = inferExpr arr st
        (idxTy, st2) = inferExpr idx st1
        st3 = checkExpectedType TyInt idxTy "array index must be int" st2
    in case arrTy of
        TyArray elemTy _ -> (elemTy, st3)
        _                -> (TyVoid, addError' ("Cannot index non-array type " <> showType arrTy) st3)

-- Field access: e.field
inferExpr (EField e fieldName) st =
    inferFieldAccess e fieldName st

-- Field access (alternative node)
inferExpr (EFieldAccess e fieldName) st =
    inferFieldAccess e fieldName st

-- Array literal: [1, 2, 3]
inferExpr (EArray []) st =
    let (tv, env') = freshTyVar (tcEnv st)
    in (TyArray tv Nothing, st { tcEnv = env' })

inferExpr (EArray (e:es)) st =
    let (elemTy, st1) = inferExpr e st
        st2 = foldl' (\s expr ->
            let (ty, s') = inferExpr expr s
            in checkAssignable elemTy ty "array element" s') st1 es
    in (TyArray elemTy Nothing, st2)

-- New: new Type[size]
inferExpr (ENew ty size) st =
    let (sizeTy, st1) = inferExpr size st
        st2 = checkExpectedType TyInt sizeTy "new array size must be int" st1
    in (TyArray ty Nothing, st2)

-- Struct literal: Person{"Alice", 25}
inferExpr (EStruct name args) st =
    case lookupStruct name (tcEnv st) of
        Nothing ->
            let st1 = foldl' (\s a -> snd (inferExpr a s)) st args
            in (TyVar name, addError' ("Undefined struct '" <> name <> "'") st1)
        Just def ->
            let fields = sdFields def
                expectedLen = length fields
                actualLen   = length args
                (argTypes, st1) = inferExprs args st
                st2 = if expectedLen /= actualLen
                      then addError' ("Struct '" <> name <> "' has "
                                     <> T.pack (show expectedLen) <> " fields but got "
                                     <> T.pack (show actualLen) <> " values") st1
                      else foldl' (\s ((fName, fType), aTy) ->
                            checkAssignable fType aTy ("struct field '" <> fName <> "'") s)
                          st1 (zip fields argTypes)
            in (TyVar name, st2)

-- | Infer field access (handles both EField and EFieldAccess)
inferFieldAccess :: Expr -> Text -> TCState -> (Type, TCState)
inferFieldAccess e fieldName st =
    let (eTy, st1) = inferExpr e st
    in case fieldName of
        -- .size is a built-in property for arrays
        "size" -> case eTy of
            TyArray _ _ -> (TyInt, st1)
            _           -> tryStructField eTy fieldName st1
        _ -> tryStructField eTy fieldName st1

-- | Try to resolve a field from a struct type
tryStructField :: Type -> Text -> TCState -> (Type, TCState)
tryStructField (TyVar structName) fieldName st =
    case lookupStruct structName (tcEnv st) of
        Nothing -> (TyVoid, addError' ("Cannot access field '" <> fieldName
                                      <> "' on type " <> showType (TyVar structName)) st)
        Just def ->
            case lookup fieldName (sdFields def) of
                Just fTy -> (fTy, st)
                Nothing  -> (TyVoid, addError' ("Struct '" <> structName
                                               <> "' has no field '" <> fieldName <> "'") st)
tryStructField ty fieldName st =
    (TyVoid, addError' ("Cannot access field '" <> fieldName
                       <> "' on type " <> showType ty) st)

-- ============================================================
-- Binary / Unary operator type rules
-- ============================================================

inferBinOp :: BinOp -> Type -> Type -> TCState -> (Type, TCState)

-- Arithmetic: both operands must be numeric, result is the "wider" type
inferBinOp OpAdd lTy rTy st = checkArith lTy rTy "+" st
inferBinOp OpSub lTy rTy st = checkArith lTy rTy "-" st
inferBinOp OpMul lTy rTy st = checkArith lTy rTy "*" st
inferBinOp OpDiv lTy rTy st = checkArith lTy rTy "/" st
inferBinOp OpMod lTy rTy st = checkArith lTy rTy "%" st

-- Comparison: both operands must be numeric, result is bool
inferBinOp OpLt  lTy rTy st = checkComparison lTy rTy "<"  st
inferBinOp OpLe  lTy rTy st = checkComparison lTy rTy "<=" st
inferBinOp OpGt  lTy rTy st = checkComparison lTy rTy ">"  st
inferBinOp OpGe  lTy rTy st = checkComparison lTy rTy ">=" st

-- Equality: both operands same type, result is bool
inferBinOp OpEq  lTy rTy st = checkEquality lTy rTy "==" st
inferBinOp OpNeq lTy rTy st = checkEquality lTy rTy "!=" st

-- Logical: both operands bool, result bool
inferBinOp OpAnd lTy rTy st = checkLogical lTy rTy "&&" st
inferBinOp OpOr  lTy rTy st = checkLogical lTy rTy "||" st

-- | Arithmetic operator check
checkArith :: Type -> Type -> Text -> TCState -> (Type, TCState)
checkArith TyInt   TyInt   _ st = (TyInt, st)
checkArith TyFloat TyFloat _ st = (TyFloat, st)
checkArith TyInt   TyFloat _ st = (TyFloat, st)
checkArith TyFloat TyInt   _ st = (TyFloat, st)
checkArith lTy     rTy     op st
    | isTyVar lTy || isTyVar rTy = (resolveNumeric lTy rTy, st) -- generic, accept
    | otherwise = (TyInt, addError' ("Operator '" <> op <> "' requires numeric operands, got "
                                    <> showType lTy <> " and " <> showType rTy) st)

-- | Comparison operator check
checkComparison :: Type -> Type -> Text -> TCState -> (Type, TCState)
checkComparison lTy rTy op st
    | isNumeric lTy && isNumeric rTy = (TyBool, st)
    | isTyVar lTy || isTyVar rTy    = (TyBool, st)
    | otherwise = (TyBool, addError' ("Operator '" <> op <> "' requires numeric operands, got "
                                     <> showType lTy <> " and " <> showType rTy) st)

-- | Equality operator check
checkEquality :: Type -> Type -> Text -> TCState -> (Type, TCState)
checkEquality lTy rTy op st
    | typesCompatible lTy rTy = (TyBool, st)
    | isTyVar lTy || isTyVar rTy = (TyBool, st)
    | otherwise = (TyBool, addError' ("Operator '" <> op <> "' requires same types, got "
                                     <> showType lTy <> " and " <> showType rTy) st)

-- | Logical operator check
checkLogical :: Type -> Type -> Text -> TCState -> (Type, TCState)
checkLogical TyBool TyBool _ st = (TyBool, st)
checkLogical lTy    rTy    op st
    | isTyVar lTy || isTyVar rTy = (TyBool, st)
    | otherwise = (TyBool, addError' ("Operator '" <> op <> "' requires bool operands, got "
                                     <> showType lTy <> " and " <> showType rTy) st)

-- | Unary operator type inference
inferUnaryOp :: UnaryOp -> Type -> TCState -> (Type, TCState)
inferUnaryOp OpNeg TyInt   st = (TyInt, st)
inferUnaryOp OpNeg TyFloat st = (TyFloat, st)
inferUnaryOp OpNeg ty      st
    | isTyVar ty = (ty, st)
    | otherwise  = (TyInt, addError' ("Unary '-' requires numeric operand, got " <> showType ty) st)

inferUnaryOp OpNot TyBool st = (TyBool, st)
inferUnaryOp OpNot ty     st
    | isTyVar ty = (TyBool, st)
    | otherwise  = (TyBool, addError' ("Unary '!' requires bool operand, got " <> showType ty) st)

-- ============================================================
-- Helpers
-- ============================================================

-- | Infer types for a list of expressions
inferExprs :: [Expr] -> TCState -> ([Type], TCState)
inferExprs [] st = ([], st)
inferExprs (e:es) st =
    let (ty, st1)  = inferExpr e st
        (tys, st2) = inferExprs es st1
    in (ty : tys, st2)

-- | Check that actual type is assignable to expected type
checkAssignable :: Type -> Type -> Text -> TCState -> TCState
checkAssignable expected actual context st
    | typesCompatible expected actual = st
    | isTyVar expected || isTyVar actual = st  -- generic types are compatible
    | otherwise = addError' ("Type mismatch in " <> context <> ": expected "
                            <> showType expected <> " but got " <> showType actual) st

-- | Check that actual type matches expected type exactly
checkExpectedType :: Type -> Type -> Text -> TCState -> TCState
checkExpectedType expected actual msg st
    | typesCompatible expected actual = st
    | isTyVar actual = st  -- inferred type variable, accept
    | otherwise = addError' (msg <> " (expected " <> showType expected
                            <> ", got " <> showType actual <> ")") st

-- | Check function call argument types
checkArgTypes :: Text -> [Maybe Type] -> [Type] -> TCState -> TCState
checkArgTypes funcName params argTypes st =
    let pairs = zip3' [1..] params argTypes
    in foldl' (\s (i, mExpected, actual) ->
        case mExpected of
            Nothing -> s  -- parameter type not annotated, accept anything
            Just expected -> checkAssignable expected actual
                ("argument " <> T.pack (show (i :: Int)) <> " of '" <> funcName <> "'") s
        ) st pairs

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' (a:as) (b:bs) (c:cs) = (a, b, c) : zip3' as bs cs
zip3' _ _ _ = []

-- | Are two types compatible?
typesCompatible :: Type -> Type -> Bool
typesCompatible TyInt TyInt       = True
typesCompatible TyFloat TyFloat   = True
typesCompatible TyString TyString = True
typesCompatible TyBool TyBool     = True
typesCompatible TyVoid TyVoid     = True
typesCompatible (TyVar _) _       = True  -- type variables unify with anything
typesCompatible _ (TyVar _)       = True
typesCompatible (TyArray t1 _) (TyArray t2 _) = typesCompatible t1 t2
typesCompatible (TyStruct n1) (TyStruct n2) = n1 == n2
typesCompatible (TyFunc p1 r1) (TyFunc p2 r2) =
    length p1 == length p2
    && all (uncurry typesCompatible) (zip p1 p2)
    && typesCompatible r1 r2
-- int/float coercion
typesCompatible TyInt TyFloat     = True
typesCompatible TyFloat TyInt     = True
typesCompatible _ _               = False

-- | Is a type numeric?
isNumeric :: Type -> Bool
isNumeric TyInt   = True
isNumeric TyFloat = True
isNumeric _       = False

-- | Is it a type variable?
isTyVar :: Type -> Bool
isTyVar (TyVar _) = True
isTyVar _         = False

-- | Resolve the "wider" numeric type
resolveNumeric :: Type -> Type -> Type
resolveNumeric TyFloat _ = TyFloat
resolveNumeric _ TyFloat = TyFloat
resolveNumeric TyInt _   = TyInt
resolveNumeric _ TyInt   = TyInt
resolveNumeric _ _       = TyInt -- fallback

-- | Show a type as human-readable text
showType :: Type -> Text
showType TyInt         = "int"
showType TyFloat       = "float"
showType TyString      = "string"
showType TyBool        = "bool"
showType TyVoid        = "void"
showType (TyVar v)     = v
showType (TyArray t _) = showType t <> "[]"
showType (TyStruct n)  = n
showType (TyFunc ps r) = "(" <> T.intercalate ", " (map showType ps) <> ") -> " <> showType r
