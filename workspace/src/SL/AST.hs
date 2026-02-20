{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Abstract Syntax Tree definitions for the SL language
module SL.AST
    ( -- * Top-level declarations
      Program(..)
    , TopLevel(..)
    , StructField(..)
    , FuncParam(..)
    , TypeVar
      -- * Statements
    , Stmt(..)
      -- * Expressions
    , Expr(..)
    , BinOp(..)
    , UnaryOp(..)
      -- * Types
    , Type(..)
      -- * Source position
    , SourcePos(..)
    , Positioned(..)
    ) where

import Data.Data (Data, Typeable)
import Data.Text (Text)

-- | Source position for error reporting
data SourcePos = SourcePos
    { posFile   :: !FilePath
    , posLine   :: !Int
    , posColumn :: !Int
    } deriving stock (Show, Eq, Ord, Data, Typeable)

-- | Wrapper for positioned AST nodes
data Positioned a = Positioned
    { position :: !SourcePos
    , unPos    :: !a
    } deriving stock (Show, Eq, Data, Typeable)

-- | Type variable name (for generics)
type TypeVar = Text

-- | A complete SL program
newtype Program = Program { topLevels :: [TopLevel] }
    deriving stock (Show, Eq, Data, Typeable)

-- | Top-level declarations: functions and structs
data TopLevel
    = TLFunc
        { funcTypeVars :: [TypeVar]        -- ^ forall a b . ...
        , funcName     :: Text             -- ^ Function name
        , funcParams   :: [FuncParam]      -- ^ Parameters
        , funcRetType  :: Maybe Type       -- ^ Return type (Nothing for inferred)
        , funcBody     :: [Stmt]           -- ^ Function body
        }
    | TLStruct
        { structName   :: Text             -- ^ Struct name
        , structFields :: [StructField]    -- ^ Fields
        }
    deriving stock (Show, Eq, Data, Typeable)

-- | Struct field definition
data StructField = StructField
    { fieldName :: Text
    , fieldType :: Type
    } deriving stock (Show, Eq, Data, Typeable)

-- | Function parameter
data FuncParam = FuncParam
    { paramName :: Text
    , paramType :: Maybe Type  -- ^ Type annotation (Nothing for inferred)
    } deriving stock (Show, Eq, Data, Typeable)

-- | Types in SL
data Type
    = TyInt                        -- ^ int
    | TyFloat                      -- ^ float
    | TyString                     -- ^ string
    | TyBool                       -- ^ bool
    | TyVoid                       -- ^ void
    | TyVar TypeVar                -- ^ Type variable (e.g., 'a', 'b')
    | TyArray Type (Maybe Expr)    -- ^ Array type: int[], int[5]
    | TyStruct Text                -- ^ Struct type
    | TyFunc [Type] Type           -- ^ Function type: (a) -> b
    deriving stock (Show, Eq, Data, Typeable)

-- | Statements
data Stmt
    = SExpr Expr                           -- ^ Expression statement
    | SLet Text (Maybe Type) (Maybe Expr)  -- ^ let x : int = 5;
    | SAssign Expr Expr                    -- ^ x = 5; or arr[i] = 5;
    | SIf Expr [Stmt] (Maybe [Stmt])       -- ^ if-else
    | SWhile Expr [Stmt]                   -- ^ while loop
    | SFor Text Expr Expr Expr [Stmt]      -- ^ for (i = 0; i < n; i++)
    | SReturn (Maybe Expr)                 -- ^ return expr;
    | SBlock [Stmt]                        -- ^ { ... }
    deriving stock (Show, Eq, Data, Typeable)

-- | Binary operators
data BinOp
    = OpAdd        -- ^ +
    | OpSub        -- ^ -
    | OpMul        -- ^ *
    | OpDiv        -- ^ /
    | OpMod        -- ^ %
    | OpEq         -- ^ ==
    | OpNeq        -- ^ !=
    | OpLt         -- ^ <
    | OpLe         -- ^ <=
    | OpGt         -- ^ >
    | OpGe         -- ^ >=
    | OpAnd        -- ^ &&
    | OpOr         -- ^ ||
    deriving stock (Show, Eq, Data, Typeable)

-- | Unary operators
data UnaryOp
    = OpNeg        -- ^ -
    | OpNot        -- ^ !
    deriving stock (Show, Eq, Data, Typeable)

-- | Expressions
data Expr
    = EInt Integer                    -- ^ Integer literal
    | EFloat Double                   -- ^ Float literal
    | EString Text                    -- ^ String literal
    | EBool Bool                      -- ^ Boolean literal
    | EVar Text                       -- ^ Variable reference
    | EBinOp BinOp Expr Expr          -- ^ Binary operation
    | EUnaryOp UnaryOp Expr           -- ^ Unary operation
    | ECall Text [Expr]               -- ^ Function call: f(x, y)
    | EIndex Expr Expr                -- ^ Array indexing: arr[i]
    | EField Expr Text                -- ^ Field access: person.name
    | EArray [Expr]                   -- ^ Array literal: [1, 2, 3]
    | ENew Type Expr                  -- ^ new int[size]
    | EStruct Text [Expr]             -- ^ Struct literal: Person{"Alice", 25}
    | EFieldAccess Expr Text          -- ^ Field access (e.g., v.size)
    deriving stock (Show, Eq, Data, Typeable)
