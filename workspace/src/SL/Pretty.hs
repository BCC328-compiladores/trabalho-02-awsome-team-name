{-# LANGUAGE OverloadedStrings #-}

-- | Pretty printer for the SL language using prettyprinter
module SL.Pretty
    ( -- * Pretty printing
      prettyProgram
    , prettyTopLevel
    , prettyStmt
    , prettyExpr
    , prettyType
      -- * Rendering
    , renderProgram
    , renderProgramAnsi
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import qualified Prettyprinter.Render.Terminal as Ansi
import Prettyprinter.Render.String (renderString)

import SL.AST

-- | Layout options for rendering
myLayoutOptions :: LayoutOptions
myLayoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1.0 }

-- | Render program to plain String
renderProgram :: Program -> String
renderProgram prog =
    renderString $ layoutPretty myLayoutOptions $ prettyProgram prog

-- | Render program with ANSI colors
renderProgramAnsi :: Program -> Text
renderProgramAnsi prog =
    renderStrict $ layoutPretty myLayoutOptions $ prettyProgramAnsi prog

-- | Pretty print a complete program
prettyProgram :: Program -> Doc ann
prettyProgram (Program tops) =
    vsep (punctuate line (map prettyTopLevel tops))

-- | Pretty print program with ANSI colors
prettyProgramAnsi :: Program -> Doc AnsiStyle
prettyProgramAnsi (Program tops) =
    vsep (punctuate line (map prettyTopLevelAnsi tops))

-- | Pretty print a top-level declaration
prettyTopLevel :: TopLevel -> Doc ann
prettyTopLevel (TLStruct name fields) =
    "struct" <+> pretty (T.unpack name) <+> lbrace <> line <>
    indent 4 (vsep (map prettyField fields)) <> line <>
    rbrace

prettyTopLevel (TLFunc tvs name params retType body) =
    forallPart <>
    "func" <+> pretty (T.unpack name) <>
    parens (hsep (punctuate comma (map prettyParam params))) <>
    retPart <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmt body)) <> line <>
    rbrace
  where
    forallPart = if null tvs
                 then emptyDoc
                 else "forall" <+> hsep (map (pretty . T.unpack) tvs) <+> "." <+> emptyDoc
    retPart = case retType of
        Nothing -> emptyDoc
        Just t  -> " :" <+> prettyType t

-- | Pretty print with ANSI colors
prettyTopLevelAnsi :: TopLevel -> Doc AnsiStyle
prettyTopLevelAnsi (TLStruct name fields) =
    annotate (Ansi.color Ansi.Blue) "struct" <+>
    annotate (Ansi.color Ansi.Green) (pretty (T.unpack name)) <+>
    lbrace <> line <>
    indent 4 (vsep (map prettyFieldAnsi fields)) <> line <>
    rbrace

prettyTopLevelAnsi (TLFunc tvs name params retType body) =
    forallPart <>
    annotate (Ansi.color Ansi.Blue) "func" <+>
    annotate (Ansi.color Ansi.Yellow) (pretty (T.unpack name)) <>
    parens (hsep (punctuate comma (map prettyParamAnsi params))) <>
    retPart <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmtAnsi body)) <> line <>
    rbrace
  where
    forallPart = if null tvs
                 then emptyDoc
                 else annotate (Ansi.color Ansi.Magenta) "forall" <+>
                      hsep (map (pretty . T.unpack) tvs) <+> "." <+> emptyDoc
    retPart = case retType of
        Nothing -> emptyDoc
        Just t  -> " :" <+> prettyTypeAnsi t

-- | Pretty print a struct field
prettyField :: StructField -> Doc ann
prettyField (StructField name ty) =
    pretty (T.unpack name) <+> ":" <+> prettyType ty <> semi

-- | Pretty print a struct field with ANSI
prettyFieldAnsi :: StructField -> Doc AnsiStyle
prettyFieldAnsi (StructField name ty) =
    pretty (T.unpack name) <+> ":" <+> prettyTypeAnsi ty <> semi

-- | Pretty print a function parameter
prettyParam :: FuncParam -> Doc ann
prettyParam (FuncParam name mty) =
    pretty (T.unpack name) <> tyPart
  where
    tyPart = case mty of
        Nothing -> emptyDoc
        Just t  -> " :" <+> prettyType t

-- | Pretty print a function parameter with ANSI
prettyParamAnsi :: FuncParam -> Doc AnsiStyle
prettyParamAnsi (FuncParam name mty) =
    pretty (T.unpack name) <> tyPart
  where
    tyPart = case mty of
        Nothing -> emptyDoc
        Just t  -> " :" <+> prettyTypeAnsi t

-- | Pretty print a type
prettyType :: Type -> Doc ann
prettyType TyInt    = "int"
prettyType TyFloat  = "float"
prettyType TyString = "string"
prettyType TyBool   = "bool"
prettyType TyVoid   = "void"
prettyType (TyVar v) = pretty (T.unpack v)
prettyType (TyArray t Nothing)  = prettyType t <> "[]"
prettyType (TyArray t (Just e)) = prettyType t <> brackets (prettyExpr e)
prettyType (TyStruct n)  = pretty (T.unpack n)
prettyType (TyFunc ps r) = parens (hsep (punctuate comma (map prettyType ps))) <+>
                           "->" <+> prettyType r

-- | Pretty print a type with ANSI
prettyTypeAnsi :: Type -> Doc AnsiStyle
prettyTypeAnsi TyInt    = annotate (Ansi.color Ansi.Cyan) "int"
prettyTypeAnsi TyFloat  = annotate (Ansi.color Ansi.Cyan) "float"
prettyTypeAnsi TyString = annotate (Ansi.color Ansi.Cyan) "string"
prettyTypeAnsi TyBool   = annotate (Ansi.color Ansi.Cyan) "bool"
prettyTypeAnsi TyVoid   = annotate (Ansi.color Ansi.Cyan) "void"
prettyTypeAnsi (TyVar v) = annotate (Ansi.color Ansi.Magenta) (pretty (T.unpack v))
prettyTypeAnsi (TyArray t Nothing)  = prettyTypeAnsi t <> "[]"
prettyTypeAnsi (TyArray t (Just e)) = prettyTypeAnsi t <> brackets (prettyExprAnsi e)
prettyTypeAnsi (TyStruct n)  = annotate (Ansi.color Ansi.Green) (pretty (T.unpack n))
prettyTypeAnsi (TyFunc ps r) = parens (hsep (punctuate comma (map prettyTypeAnsi ps))) <+>
                               "->" <+> prettyTypeAnsi r

-- | Pretty print a statement
prettyStmt :: Stmt -> Doc ann
prettyStmt (SExpr e) = prettyExpr e <> semi

prettyStmt (SLet name mty mval) =
    "let" <+> pretty (T.unpack name) <> tyPart <> valPart <> semi
  where
    tyPart = case mty of
        Nothing -> emptyDoc
        Just t  -> " :" <+> prettyType t
    valPart = case mval of
        Nothing -> emptyDoc
        Just e  -> " =" <+> prettyExpr e

prettyStmt (SAssign lhs rhs) =
    prettyExpr lhs <+> "=" <+> prettyExpr rhs <> semi

prettyStmt (SIf cond thenB elseB) =
    "if" <+> parens (prettyExpr cond) <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmt thenB)) <> line <>
    rbrace <> elsePart
  where
    elsePart = case elseB of
        Nothing -> emptyDoc
        Just b  -> " else" <+> lbrace <> line <>
                   indent 4 (vsep (map prettyStmt b)) <> line <>
                   rbrace

prettyStmt (SWhile cond body) =
    "while" <+> parens (prettyExpr cond) <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmt body)) <> line <>
    rbrace

prettyStmt (SFor var init' cond update body) =
    "for" <+> parens (pretty (T.unpack var) <+> "=" <+> prettyExpr init' <> semi <+>
                      prettyExpr cond <> semi <+> prettyExpr update) <+>
    lbrace <> line <>
    indent 4 (vsep (map prettyStmt body)) <> line <>
    rbrace

prettyStmt (SReturn Nothing) = "return" <> semi

prettyStmt (SReturn (Just e)) = "return" <+> prettyExpr e <> semi

prettyStmt (SBlock stmts) =
    lbrace <> line <>
    indent 4 (vsep (map prettyStmt stmts)) <> line <>
    rbrace

-- | Pretty print a statement with ANSI
prettyStmtAnsi :: Stmt -> Doc AnsiStyle
prettyStmtAnsi (SExpr e) = prettyExprAnsi e <> semi

prettyStmtAnsi (SLet name mty mval) =
    annotate (Ansi.color Ansi.Blue) "let" <+>
    pretty (T.unpack name) <> tyPart <> valPart <> semi
  where
    tyPart = case mty of
        Nothing -> emptyDoc
        Just t  -> " :" <+> prettyTypeAnsi t
    valPart = case mval of
        Nothing -> emptyDoc
        Just e  -> " =" <+> prettyExprAnsi e

prettyStmtAnsi (SAssign lhs rhs) =
    prettyExprAnsi lhs <+> "=" <+> prettyExprAnsi rhs <> semi

prettyStmtAnsi (SIf cond thenB elseB) =
    annotate (Ansi.color Ansi.Blue) "if" <+>
    parens (prettyExprAnsi cond) <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmtAnsi thenB)) <> line <>
    rbrace <> elsePart
  where
    elsePart = case elseB of
        Nothing -> emptyDoc
        Just b  -> " " <> annotate (Ansi.color Ansi.Blue) "else" <+> lbrace <> line <>
                   indent 4 (vsep (map prettyStmtAnsi b)) <> line <>
                   rbrace

prettyStmtAnsi (SWhile cond body) =
    annotate (Ansi.color Ansi.Blue) "while" <+>
    parens (prettyExprAnsi cond) <+> lbrace <> line <>
    indent 4 (vsep (map prettyStmtAnsi body)) <> line <>
    rbrace

prettyStmtAnsi (SFor var init' cond update body) =
    annotate (Ansi.color Ansi.Blue) "for" <+>
    parens (pretty (T.unpack var) <+> "=" <+> prettyExprAnsi init' <> semi <+>
            prettyExprAnsi cond <> semi <+> prettyExprAnsi update) <+>
    lbrace <> line <>
    indent 4 (vsep (map prettyStmtAnsi body)) <> line <>
    rbrace

prettyStmtAnsi (SReturn Nothing) =
    annotate (Ansi.color Ansi.Blue) "return" <> semi

prettyStmtAnsi (SReturn (Just e)) =
    annotate (Ansi.color Ansi.Blue) "return" <+> prettyExprAnsi e <> semi

prettyStmtAnsi (SBlock stmts) =
    lbrace <> line <>
    indent 4 (vsep (map prettyStmtAnsi stmts)) <> line <>
    rbrace

-- | Pretty print an expression
prettyExpr :: Expr -> Doc ann
prettyExpr (EInt n)    = pretty n
prettyExpr (EFloat f)  = pretty f
prettyExpr (EString s) = dquotes (pretty (T.unpack s))
prettyExpr (EBool b)   = if b then "true" else "false"
prettyExpr (EVar name) = pretty (T.unpack name)

prettyExpr (EBinOp op l r) =
    prettyExprPrec l <+> prettyBinOp op <+> prettyExprPrec r

prettyExpr (EUnaryOp op e) = prettyUnaryOp op <> prettyExprPrec e

prettyExpr (ECall name args) =
    pretty (T.unpack name) <> parens (hsep (punctuate comma (map prettyExpr args)))

prettyExpr (EIndex arr idx) =
    prettyExprPrec arr <> brackets (prettyExpr idx)

prettyExpr (EField e name) =
    prettyExprPrec e <> "." <> pretty (T.unpack name)

prettyExpr (EArray elems) =
    brackets (hsep (punctuate comma (map prettyExpr elems)))

prettyExpr (ENew ty size) =
    "new" <+> prettyType ty <> brackets (prettyExpr size)

prettyExpr (EStruct name fields) =
    pretty (T.unpack name) <> braces (hsep (punctuate comma (map prettyExpr fields)))

prettyExpr (EFieldAccess e name) =
    prettyExprPrec e <> "." <> pretty (T.unpack name)

-- | Pretty print expression with ANSI colors
prettyExprAnsi :: Expr -> Doc AnsiStyle
prettyExprAnsi (EInt n)    = annotate (Ansi.color Ansi.Red) (pretty n)
prettyExprAnsi (EFloat f)  = annotate (Ansi.color Ansi.Red) (pretty f)
prettyExprAnsi (EString s) = annotate (Ansi.color Ansi.Green) (dquotes (pretty (T.unpack s)))
prettyExprAnsi (EBool b)   = annotate (Ansi.color Ansi.Magenta) (if b then "true" else "false")
prettyExprAnsi (EVar name) = pretty (T.unpack name)

prettyExprAnsi (EBinOp op l r) =
    prettyExprPrecAnsi l <+> prettyBinOp op <+> prettyExprPrecAnsi r

prettyExprAnsi (EUnaryOp op e) = prettyUnaryOp op <> prettyExprPrecAnsi e

prettyExprAnsi (ECall name args) =
    annotate (Ansi.color Ansi.Yellow) (pretty (T.unpack name)) <>
    parens (hsep (punctuate comma (map prettyExprAnsi args)))

prettyExprAnsi (EIndex arr idx) =
    prettyExprPrecAnsi arr <> brackets (prettyExprAnsi idx)

prettyExprAnsi (EField e name) =
    prettyExprPrecAnsi e <> "." <> pretty (T.unpack name)

prettyExprAnsi (EArray elems) =
    brackets (hsep (punctuate comma (map prettyExprAnsi elems)))

prettyExprAnsi (ENew ty size) =
    annotate (Ansi.color Ansi.Blue) "new" <+> prettyTypeAnsi ty <> brackets (prettyExprAnsi size)

prettyExprAnsi (EStruct name fields) =
    annotate (Ansi.color Ansi.Green) (pretty (T.unpack name)) <>
    braces (hsep (punctuate comma (map prettyExprAnsi fields)))

prettyExprAnsi (EFieldAccess e name) =
    prettyExprPrecAnsi e <> "." <> pretty (T.unpack name)

-- | Pretty print expression with precedence handling
prettyExprPrec :: Expr -> Doc ann
prettyExprPrec e@(EBinOp _ _ _) = parens (prettyExpr e)
prettyExprPrec e                = prettyExpr e

-- | Pretty print expression with precedence handling (ANSI)
prettyExprPrecAnsi :: Expr -> Doc AnsiStyle
prettyExprPrecAnsi e@(EBinOp _ _ _) = parens (prettyExprAnsi e)
prettyExprPrecAnsi e                = prettyExprAnsi e

-- | Pretty print a binary operator
prettyBinOp :: BinOp -> Doc ann
prettyBinOp OpAdd = "+"
prettyBinOp OpSub = "-"
prettyBinOp OpMul = "*"
prettyBinOp OpDiv = "/"
prettyBinOp OpMod = "%"
prettyBinOp OpEq  = "=="
prettyBinOp OpNeq = "!="
prettyBinOp OpLt  = "<"
prettyBinOp OpLe  = "<="
prettyBinOp OpGt  = ">"
prettyBinOp OpGe  = ">="
prettyBinOp OpAnd = "&&"
prettyBinOp OpOr  = "||"

-- | Pretty print a unary operator
prettyUnaryOp :: UnaryOp -> Doc ann
prettyUnaryOp OpNeg = "-"
prettyUnaryOp OpNot = "!"
