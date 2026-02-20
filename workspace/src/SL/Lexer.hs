{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the SL language using Megaparsec
module SL.Lexer
    ( -- * Parser type
      Parser
      -- * Tokens
    , Token(..)
    , TokenPos(..)
      -- * Lexer utilities
    , sc
    , lexeme
    , symbol
      -- * Literals
    , intLiteral
    , floatLiteral
    , stringLiteral
    , boolLiteral
      -- * Identifiers and reserved words
    , identifier
    , reserved
    , reservedWords
      -- * Operators
    , operator
      -- * Punctuation
    , parens
    , braces
    , brackets
    , semi
    , comma
    , colon
    , dot
    , arrow
      -- * Tokenizer
    , tokenize
    , runLexer
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type alias
type Parser = Parsec Void Text

-- | Token with position information
data TokenPos = TokenPos
    { tokPos   :: !(Int, Int)  -- ^ (line, column)
    , tokValue :: !Token
    } deriving (Show, Eq)

-- | Token types
data Token
    = TokInt Integer
    | TokFloat Double
    | TokString Text
    | TokBool Bool
    | TokIdent Text
    | TokReserved Text
    | TokOperator Text
    | TokLParen
    | TokRParen
    | TokLBrace
    | TokRBrace
    | TokLBracket
    | TokRBracket
    | TokSemi
    | TokComma
    | TokColon
    | TokDot
    | TokArrow
    | TokAssign
    | TokEOF
    deriving (Show, Eq)

-- | Reserved words in SL
reservedWords :: [Text]
reservedWords =
    [ "func", "struct", "if", "else", "while", "for"
    , "return", "let", "new", "forall"
    , "int", "float", "string", "bool", "void"
    , "true", "false"
    ]

-- | Skip whitespace and comments
sc :: Parser ()
sc = L.space
    space1                          -- spaces
    (L.skipLineComment "//")        -- C-style line comments
    (L.skipBlockComment "/*" "*/")  -- block comments

-- | Lexeme wrapper (consumes trailing whitespace)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser (specific string with trailing whitespace)
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse an integer literal
intLiteral :: Parser Integer
intLiteral = lexeme L.decimal

-- | Parse a float literal
floatLiteral :: Parser Double
floatLiteral = lexeme L.float

-- | Parse a string literal
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    return $ T.pack content

-- | Parse a boolean literal
boolLiteral :: Parser Bool
boolLiteral = lexeme $ choice
    [ True  <$ string "true"
    , False <$ string "false"
    ]

-- | Parse an identifier (not a reserved word)
identifier :: Parser Text
identifier = lexeme $ try $ do
    ident <- identRaw
    if ident `elem` reservedWords
        then fail $ "keyword " ++ T.unpack ident ++ " cannot be an identifier"
        else return ident

-- | Raw identifier parser
identRaw :: Parser Text
identRaw = do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    return $ T.pack (first : rest)

-- | Parse a reserved word
reserved :: Text -> Parser Text
reserved w = lexeme $ try $ do
    _ <- string w
    notFollowedBy alphaNumChar
    return w

-- | Parse an operator
operator :: Text -> Parser Text
operator = symbol

-- | Parse something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse something between braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse something between brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Semicolon
semi :: Parser Text
semi = symbol ";"

-- | Comma
comma :: Parser Text
comma = symbol ","

-- | Colon
colon :: Parser Text
colon = symbol ":"

-- | Dot
dot :: Parser Text
dot = symbol "."

-- | Arrow
arrow :: Parser Text
arrow = symbol "->"

-- | Tokenize a single token with position
tokenWithPos :: Parser TokenPos
tokenWithPos = do
    pos <- getSourcePos
    tok <- pToken
    return $ TokenPos (unPos $ sourceLine pos, unPos $ sourceColumn pos) tok

-- | Parse a single token
pToken :: Parser Token
pToken = choice
    [ TokArrow    <$  try (symbol "->")
    , TokOperator <$> try (symbol "<=")
    , TokOperator <$> try (symbol ">=")
    , TokOperator <$> try (symbol "==")
    , TokOperator <$> try (symbol "!=")
    , TokOperator <$> try (symbol "&&")
    , TokOperator <$> try (symbol "||")
    , TokOperator <$> symbol "+"
    , TokOperator <$> symbol "-"
    , TokOperator <$> symbol "*"
    , TokOperator <$> symbol "/"
    , TokOperator <$> symbol "%"
    , TokOperator <$> symbol "<"
    , TokOperator <$> symbol ">"
    , TokOperator <$> symbol "!"
    , TokAssign   <$  symbol "="
    , TokLParen   <$  symbol "("
    , TokRParen   <$  symbol ")"
    , TokLBrace   <$  symbol "{"
    , TokRBrace   <$  symbol "}"
    , TokLBracket <$  symbol "["
    , TokRBracket <$  symbol "]"
    , TokSemi     <$  symbol ";"
    , TokComma    <$  symbol ","
    , TokColon    <$  symbol ":"
    , TokDot      <$  symbol "."
    , TokBool     <$> try boolLiteral
    , try (TokFloat <$> floatLiteral)
    , TokInt      <$> intLiteral
    , TokString   <$> stringLiteral
    , pIdentOrReserved
    ]

-- | Parse identifier or reserved word
pIdentOrReserved :: Parser Token
pIdentOrReserved = lexeme $ do
    ident <- identRaw
    return $ if ident `elem` reservedWords
             then TokReserved ident
             else TokIdent ident

-- | Tokenize entire input
tokenize :: Parser [TokenPos]
tokenize = do
    sc  -- skip leading whitespace
    toks <- many tokenWithPos
    eof
    pos <- getSourcePos
    let eofPos = TokenPos (unPos $ sourceLine pos, unPos $ sourceColumn pos) TokEOF
    return (toks ++ [eofPos])

-- | Run the lexer on input text
runLexer :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [TokenPos]
runLexer = parse tokenize
