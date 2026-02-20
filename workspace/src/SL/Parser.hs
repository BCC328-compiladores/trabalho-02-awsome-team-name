{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the SL language using Megaparsec
module SL.Parser
    ( -- * Parser functions
      parseProgram
    , parseExpr
    , parseStmt
    , parseType
      -- * Running the parser
    , runParser
    , runParserExpr
      -- * AST to Tree conversion
    , programToTree
    ) where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Data.Void (Void)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec as MP

import SL.AST
import SL.Lexer

-- | Parse a complete SL program
parseProgram :: Parser Program
parseProgram = do
    sc  -- skip initial whitespace
    tops <- many parseTopLevel
    eof
    return $ Program tops

-- | Parse a top-level declaration
parseTopLevel :: Parser TopLevel
parseTopLevel = choice
    [ parseStruct
    , parseFunction
    ]

-- | Parse a struct declaration
parseStruct :: Parser TopLevel
parseStruct = do
    _ <- reserved "struct"
    name <- identifier
    fields <- braces (many parseStructField)
    return $ TLStruct name fields

-- | Parse a struct field
parseStructField :: Parser StructField
parseStructField = do
    name <- identifier
    _ <- colon
    ty <- parseType
    _ <- semi
    return $ StructField name ty

-- | Parse a function declaration
parseFunction :: Parser TopLevel
parseFunction = do
    -- Optional forall clause for generics
    typeVars <- option [] parseForall
    _ <- reserved "func"
    name <- identifier
    params <- parens (parseParam `sepBy` comma)
    -- Optional return type
    retType <- optional $ do
        _ <- colon
        parseType
    body <- braces (many parseStmt)
    return $ TLFunc typeVars name params retType body

-- | Parse forall clause: forall a b .
parseForall :: Parser [TypeVar]
parseForall = do
    _ <- reserved "forall"
    vars <- some identifier
    _ <- symbol "."
    return vars

-- | Parse a function parameter
parseParam :: Parser FuncParam
parseParam = do
    name <- identifier
    ty <- optional $ do
        _ <- colon
        parseType
    return $ FuncParam name ty

-- | Parse a type
parseType :: Parser Type
parseType = do
    base <- parseBaseType
    -- Check for array suffix
    arrays <- many parseArraySuffix
    return $ foldl (\t arr -> TyArray t arr) base arrays

-- | Parse array suffix: [] or [5]
parseArraySuffix :: Parser (Maybe Expr)
parseArraySuffix = do
    _ <- symbol "["
    size <- optional parseExpr
    _ <- symbol "]"
    return size

-- | Parse a base type (without array suffix)
parseBaseType :: Parser Type
parseBaseType = choice
    [ TyInt    <$ reserved "int"
    , TyFloat  <$ reserved "float"
    , TyString <$ reserved "string"
    , TyBool   <$ reserved "bool"
    , TyVoid   <$ reserved "void"
    , parseFuncType
    , TyVar <$> identifier  -- Type variable or struct name
    ]

-- | Parse a function type: (a) -> b
parseFuncType :: Parser Type
parseFuncType = try $ do
    paramTypes <- parens (parseType `sepBy` comma)
    _ <- arrow
    retType <- parseType
    return $ TyFunc paramTypes retType

-- | Parse a statement
parseStmt :: Parser Stmt
parseStmt = choice
    [ parseReturn
    , parseLet
    , parseIf
    , parseWhile
    , parseFor
    , parseBlock
    , parseExprOrAssignStmt
    ]

-- | Parse a return statement
parseReturn :: Parser Stmt
parseReturn = do
    _ <- reserved "return"
    expr <- optional parseExpr
    _ <- semi
    return $ SReturn expr

-- | Parse a let statement
parseLet :: Parser Stmt
parseLet = do
    _ <- reserved "let"
    name <- identifier
    ty <- optional $ do
        _ <- colon
        parseType
    val <- optional $ do
        _ <- symbol "="
        parseExpr
    _ <- semi
    return $ SLet name ty val

-- | Parse an if statement
parseIf :: Parser Stmt
parseIf = do
    _ <- reserved "if"
    cond <- parens parseExpr
    thenBlock <- braces (many parseStmt)
    elseBlock <- optional $ do
        _ <- reserved "else"
        braces (many parseStmt)
    return $ SIf cond thenBlock elseBlock

-- | Parse a while statement
parseWhile :: Parser Stmt
parseWhile = do
    _ <- reserved "while"
    cond <- parens parseExpr
    body <- braces (many parseStmt)
    return $ SWhile cond body

-- | Parse a for statement: for (i = 0; i < n; i++)
parseFor :: Parser Stmt
parseFor = do
    _ <- reserved "for"
    _ <- symbol "("
    var <- identifier
    _ <- symbol "="
    initExpr <- parseExpr
    _ <- semi
    condExpr <- parseExpr
    _ <- semi
    updateExpr <- parseExpr
    _ <- symbol ")"
    body <- braces (many parseStmt)
    return $ SFor var initExpr condExpr updateExpr body

-- | Parse a block statement
parseBlock :: Parser Stmt
parseBlock = SBlock <$> braces (many parseStmt)

-- | Parse an expression statement or assignment
parseExprOrAssignStmt :: Parser Stmt
parseExprOrAssignStmt = do
    expr <- parseExpr
    choice
        [ do
            _ <- symbol "="
            rhs <- parseExpr
            _ <- semi
            return $ SAssign expr rhs
        , do
            _ <- semi
            return $ SExpr expr
        ]

-- | Parse an expression with operator precedence
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

-- | Operator precedence table
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Prefix (EUnaryOp OpNeg <$ symbol "-")
      , Prefix (EUnaryOp OpNot <$ symbol "!")
      ]
    , [ InfixL (EBinOp OpMul <$ symbol "*")
      , InfixL (EBinOp OpDiv <$ symbol "/")
      , InfixL (EBinOp OpMod <$ symbol "%")
      ]
    , [ InfixL (EBinOp OpAdd <$ symbol "+")
      , InfixL (EBinOp OpSub <$ try (symbol "-" <* notFollowedBy (char '>')))
      ]
    , [ InfixN (EBinOp OpLe <$ try (symbol "<="))
      , InfixN (EBinOp OpGe <$ try (symbol ">="))
      , InfixN (EBinOp OpLt <$ symbol "<")
      , InfixN (EBinOp OpGt <$ symbol ">")
      ]
    , [ InfixN (EBinOp OpEq  <$ try (symbol "=="))
      , InfixN (EBinOp OpNeq <$ try (symbol "!="))
      ]
    , [ InfixL (EBinOp OpAnd <$ try (symbol "&&"))
      ]
    , [ InfixL (EBinOp OpOr  <$ try (symbol "||"))
      ]
    ]

-- | Parse a term (primary expression with postfix operations)
parseTerm :: Parser Expr
parseTerm = do
    base <- parsePrimary
    postfixes <- many parsePostfix
    return $ foldl applyPostfix base postfixes

-- | Postfix operation
data Postfix
    = PostIndex Expr      -- [expr]
    | PostField Text      -- .field
    | PostCall [Expr]     -- (args)

-- | Apply a postfix operation
applyPostfix :: Expr -> Postfix -> Expr
applyPostfix e (PostIndex idx)  = EIndex e idx
applyPostfix e (PostField name) = EField e name
applyPostfix (EVar name) (PostCall args) = ECall name args
applyPostfix e (PostCall args)  = ECall (exprToName e) args
  where
    exprToName (EVar n) = n
    exprToName _        = "anonymous"

-- | Parse postfix operations
parsePostfix :: Parser Postfix
parsePostfix = choice
    [ PostIndex <$> brackets parseExpr
    , PostField <$> (dot *> identifier)
    , PostCall  <$> parens (parseExpr `sepBy` comma)
    ]

-- | Parse a primary expression
parsePrimary :: Parser Expr
parsePrimary = choice
    [ parseNew
    , parseLiteral
    , parseArrayLiteral
    , parseParenExpr
    , parseIdentOrStruct
    ]

-- | Parse a new expression: new int[size]
parseNew :: Parser Expr
parseNew = do
    _ <- reserved "new"
    ty <- parseBaseType
    _ <- symbol "["
    size <- parseExpr
    _ <- symbol "]"
    return $ ENew ty size

-- | Parse a literal
parseLiteral :: Parser Expr
parseLiteral = choice
    [ EBool <$> try boolLiteral
    , try (EFloat <$> floatLiteral)
    , EInt <$> intLiteral
    , EString <$> stringLiteral
    ]

-- | Parse an array literal: [1, 2, 3]
parseArrayLiteral :: Parser Expr
parseArrayLiteral = EArray <$> brackets (parseExpr `sepBy` comma)

-- | Parse a parenthesized expression
parseParenExpr :: Parser Expr
parseParenExpr = parens parseExpr

-- | Parse an identifier, function call, or struct literal
parseIdentOrStruct :: Parser Expr
parseIdentOrStruct = do
    name <- identifier
    choice
        [ -- Struct literal: Person{"Alice", 25}
          EStruct name <$> braces (parseExpr `sepBy` comma)
        , return $ EVar name
        ]

-- | Run the parser on input
runParser :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Program
runParser = MP.parse parseProgram

-- | Run the parser on a single expression
runParserExpr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Expr
runParserExpr = MP.parse (sc *> parseExpr <* eof)

-- | Convert a Program to a Tree for visualization
programToTree :: Program -> Tree String
programToTree (Program tops) = Node "Program" (map topLevelToTree tops)

-- | Convert a TopLevel to a Tree
topLevelToTree :: TopLevel -> Tree String
topLevelToTree (TLFunc tvs name params retType body) =
    Node ("func " ++ T.unpack name ++ tvInfo ++ retInfo)
        (paramTrees ++ stmtTrees)
  where
    tvInfo = if null tvs
             then ""
             else " [forall " ++ unwords (map T.unpack tvs) ++ "]"
    retInfo = case retType of
        Nothing -> ""
        Just t  -> " : " ++ typeToString t
    paramTrees = map paramToTree params
    stmtTrees = map stmtToTree body

topLevelToTree (TLStruct name fields) =
    Node ("struct " ++ T.unpack name) (map fieldToTree fields)

-- | Convert a FuncParam to a Tree
paramToTree :: FuncParam -> Tree String
paramToTree (FuncParam name mty) =
    Node ("param " ++ T.unpack name ++ tyInfo) []
  where
    tyInfo = case mty of
        Nothing -> ""
        Just t  -> " : " ++ typeToString t

-- | Convert a StructField to a Tree
fieldToTree :: StructField -> Tree String
fieldToTree (StructField name ty) =
    Node ("field " ++ T.unpack name ++ " : " ++ typeToString ty) []

-- | Convert a Statement to a Tree
stmtToTree :: Stmt -> Tree String
stmtToTree (SExpr e) = Node "expr" [exprToTree e]
stmtToTree (SLet name mty mval) =
    Node ("let " ++ T.unpack name ++ tyInfo) valTree
  where
    tyInfo = case mty of
        Nothing -> ""
        Just t  -> " : " ++ typeToString t
    valTree = case mval of
        Nothing -> []
        Just e  -> [exprToTree e]
stmtToTree (SAssign lhs rhs) =
    Node "assign" [exprToTree lhs, exprToTree rhs]
stmtToTree (SIf cond thenB elseB) =
    Node "if" $ [Node "cond" [exprToTree cond], Node "then" (map stmtToTree thenB)]
             ++ maybe [] (\b -> [Node "else" (map stmtToTree b)]) elseB
stmtToTree (SWhile cond body) =
    Node "while" [Node "cond" [exprToTree cond], Node "body" (map stmtToTree body)]
stmtToTree (SFor var init' cond update body) =
    Node ("for " ++ T.unpack var)
        [ Node "init" [exprToTree init']
        , Node "cond" [exprToTree cond]
        , Node "update" [exprToTree update]
        , Node "body" (map stmtToTree body)
        ]
stmtToTree (SReturn me) =
    Node "return" $ maybe [] (\e -> [exprToTree e]) me
stmtToTree (SBlock stmts) =
    Node "block" (map stmtToTree stmts)

-- | Convert an Expression to a Tree
exprToTree :: Expr -> Tree String
exprToTree (EInt n)        = Node ("int " ++ show n) []
exprToTree (EFloat f)      = Node ("float " ++ show f) []
exprToTree (EString s)     = Node ("string \"" ++ T.unpack s ++ "\"") []
exprToTree (EBool b)       = Node ("bool " ++ show b) []
exprToTree (EVar name)     = Node ("var " ++ T.unpack name) []
exprToTree (EBinOp op l r) = Node ("binop " ++ showOp op) [exprToTree l, exprToTree r]
exprToTree (EUnaryOp op e) = Node ("unary " ++ showUnaryOp op) [exprToTree e]
exprToTree (ECall name args) =
    Node ("call " ++ T.unpack name) (map exprToTree args)
exprToTree (EIndex arr idx) =
    Node "index" [exprToTree arr, exprToTree idx]
exprToTree (EField e name) =
    Node ("field " ++ T.unpack name) [exprToTree e]
exprToTree (EArray elems) =
    Node "array" (map exprToTree elems)
exprToTree (ENew ty size) =
    Node ("new " ++ typeToString ty) [exprToTree size]
exprToTree (EStruct name fields) =
    Node ("struct " ++ T.unpack name) (map exprToTree fields)
exprToTree (EFieldAccess e name) =
    Node ("fieldAccess " ++ T.unpack name) [exprToTree e]

-- | Show binary operator
showOp :: BinOp -> String
showOp OpAdd = "+"
showOp OpSub = "-"
showOp OpMul = "*"
showOp OpDiv = "/"
showOp OpMod = "%"
showOp OpEq  = "=="
showOp OpNeq = "!="
showOp OpLt  = "<"
showOp OpLe  = "<="
showOp OpGt  = ">"
showOp OpGe  = ">="
showOp OpAnd = "&&"
showOp OpOr  = "||"

-- | Show unary operator
showUnaryOp :: UnaryOp -> String
showUnaryOp OpNeg = "-"
showUnaryOp OpNot = "!"

-- | Convert Type to String
typeToString :: Type -> String
typeToString TyInt         = "int"
typeToString TyFloat       = "float"
typeToString TyString      = "string"
typeToString TyBool        = "bool"
typeToString TyVoid        = "void"
typeToString (TyVar v)     = T.unpack v
typeToString (TyArray t Nothing)  = typeToString t ++ "[]"
typeToString (TyArray t (Just e)) = typeToString t ++ "[" ++ exprShow e ++ "]"
typeToString (TyStruct n)  = T.unpack n
typeToString (TyFunc ps r) = "(" ++ unwords (map typeToString ps) ++ ") -> " ++ typeToString r

-- | Simple expression to string (for array sizes)
exprShow :: Expr -> String
exprShow (EInt n)  = show n
exprShow (EVar v)  = T.unpack v
exprShow _         = "..."
