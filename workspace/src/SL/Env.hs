{-# LANGUAGE OverloadedStrings #-}

-- | Environment and symbol table for semantic analysis of the SL language.
--
-- Provides scoped symbol tables for variables, functions, and struct types,
-- along with fresh type variable generation for type inference.
module SL.Env
    ( -- * Environment types
      Env(..)
    , FuncSig(..)
    , StructDef(..)
      -- * Environment operations
    , emptyEnv
    , lookupVar
    , lookupVarCurrentScope
    , insertVar
    , lookupFunc
    , insertFunc
    , lookupStruct
    , insertStruct
    , pushScope
    , popScope
      -- * Type variable generation
    , freshTyVar
      -- * Semantic errors
    , SemanticError(..)
    , prettyError
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SL.AST (Type(..), TypeVar)

-- | A semantic error with position information
data SemanticError = SemanticError
    { errLine    :: !Int
    , errColumn  :: !Int
    , errMessage :: !Text
    } deriving (Show, Eq)

-- | Pretty-print a semantic error
prettyError :: SemanticError -> String
prettyError (SemanticError line col msg) =
    "Semantic error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ T.unpack msg

-- | Function signature: type variables, parameter types, return type
data FuncSig = FuncSig
    { fsTypeVars  :: ![TypeVar]      -- ^ Generic type variables (forall a b .)
    , fsParamTypes :: ![Maybe Type]  -- ^ Parameter types (Nothing = to be inferred)
    , fsRetType   :: !(Maybe Type)   -- ^ Return type (Nothing = to be inferred)
    } deriving (Show, Eq)

-- | Struct definition: name and ordered list of (fieldName, fieldType)
data StructDef = StructDef
    { sdName   :: !Text
    , sdFields :: ![(Text, Type)]
    } deriving (Show, Eq)

-- | The type-checking environment.
--
-- Variable scopes are represented as a stack of Maps (head = innermost scope).
-- Functions and structs are global (single scope).
data Env = Env
    { envVarScopes  :: ![Map Text Type]    -- ^ Stack of variable scopes
    , envFuncs      :: !(Map Text FuncSig) -- ^ Global function signatures
    , envStructs    :: !(Map Text StructDef) -- ^ Global struct definitions
    , envFreshCount :: !Int                -- ^ Counter for generating fresh type variables
    } deriving (Show)

-- | An empty environment with one (global) scope
emptyEnv :: Env
emptyEnv = Env
    { envVarScopes  = [Map.empty]
    , envFuncs      = Map.empty
    , envStructs    = Map.empty
    , envFreshCount = 0
    }

-- | Look up a variable in the nearest enclosing scope
lookupVar :: Text -> Env -> Maybe Type
lookupVar name env = go (envVarScopes env)
  where
    go []     = Nothing
    go (s:ss) = case Map.lookup name s of
        Just ty -> Just ty
        Nothing -> go ss

-- | Look up a variable only in the current (innermost) scope
lookupVarCurrentScope :: Text -> Env -> Maybe Type
lookupVarCurrentScope name env =
    case envVarScopes env of
        []    -> Nothing
        (s:_) -> Map.lookup name s

-- | Insert a variable into the current (innermost) scope
insertVar :: Text -> Type -> Env -> Env
insertVar name ty env =
    case envVarScopes env of
        []     -> env { envVarScopes = [Map.singleton name ty] }
        (s:ss) -> env { envVarScopes = Map.insert name ty s : ss }

-- | Look up a function signature
lookupFunc :: Text -> Env -> Maybe FuncSig
lookupFunc name env = Map.lookup name (envFuncs env)

-- | Insert a function signature
insertFunc :: Text -> FuncSig -> Env -> Env
insertFunc name sig env =
    env { envFuncs = Map.insert name sig (envFuncs env) }

-- | Look up a struct definition
lookupStruct :: Text -> Env -> Maybe StructDef
lookupStruct name env = Map.lookup name (envStructs env)

-- | Insert a struct definition
insertStruct :: Text -> StructDef -> Env -> Env
insertStruct name def env =
    env { envStructs = Map.insert name def (envStructs env) }

-- | Push a new (empty) scope onto the scope stack
pushScope :: Env -> Env
pushScope env = env { envVarScopes = Map.empty : envVarScopes env }

-- | Pop the innermost scope. If only one scope remains, it is kept.
popScope :: Env -> Env
popScope env = case envVarScopes env of
    []     -> env
    [s]    -> env  -- keep global scope
    (_:ss) -> env { envVarScopes = ss }

-- | Generate a fresh type variable name (e.g., _t0, _t1, ...)
freshTyVar :: Env -> (Type, Env)
freshTyVar env =
    let n = envFreshCount env
        tv = TyVar (T.pack ("_t" ++ show n))
    in (tv, env { envFreshCount = n + 1 })
