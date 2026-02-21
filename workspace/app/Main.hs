{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (intercalate)
import qualified Data.Map.Strict
import Data.Tree (drawTree)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty)

import SL.AST (Type(..))
import SL.Lexer (runLexer, TokenPos(..), Token(..))
import SL.Parser (runParser, programToTree)
import SL.Pretty (renderProgram)
import SL.TypeChecker (typeCheck, TCResult(..))
import SL.Env (prettyError)
import SL.Interpreter (interpret, prettyRuntimeError)

-- | Main entry point
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            printUsage
            exitFailure
        Right (mode, filePath) -> do
            content <- TIO.readFile filePath
            runMode mode filePath content

-- | CLI mode
data Mode
    = ModeLexer      -- ^ --lexer: Print tokens with positions
    | ModeParser     -- ^ --parser: Print AST tree
    | ModePretty     -- ^ --pretty: Print reconstructed source
    | ModeTypeCheck  -- ^ --typecheck: Run semantic analysis
    | ModeInterpret  -- ^ --interpret: Run the program
    deriving (Show, Eq)

-- | Parse command line arguments
parseArgs :: [String] -> Either String (Mode, FilePath)
parseArgs args = case args of
    ["--lexer", file]      -> Right (ModeLexer, file)
    ["--parser", file]     -> Right (ModeParser, file)
    ["--pretty", file]     -> Right (ModePretty, file)
    ["--typecheck", file]  -> Right (ModeTypeCheck, file)
    ["--interpret", file]  -> Right (ModeInterpret, file)
    ["--run", file]        -> Right (ModeInterpret, file)
    [file]                 -> Right (ModeParser, file)  -- default to parser
    []                     -> Left "No input file specified"
    _                      -> Left "Invalid arguments"

-- | Print usage information
printUsage :: IO ()
printUsage = do
    putStrLn "Usage: sl-compiler [OPTIONS] <file>"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --lexer      Print tokens with line and column information"
    putStrLn "  --parser     Print the AST as a tree (default)"
    putStrLn "  --pretty     Print reconstructed source code"
    putStrLn "  --typecheck  Run semantic analysis and type checking"
    putStrLn "  --interpret  Parse, type-check, and execute the program"
    putStrLn "  --run        Alias for --interpret"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  sl-compiler --lexer  input.sl"
    putStrLn "  sl-compiler --parser input.sl"
    putStrLn "  sl-compiler --pretty input.sl"
    putStrLn "  sl-compiler --interpret input.sl"

-- | Run the compiler in the specified mode
runMode :: Mode -> FilePath -> Text -> IO ()
runMode ModeLexer filePath content = do
    case runLexer filePath content of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right tokens -> do
            putStrLn "Tokens:"
            putStrLn (replicate 60 '-')
            mapM_ printToken tokens
            exitSuccess

runMode ModeParser filePath content = do
    case runParser filePath content of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right program -> do
            putStrLn "Abstract Syntax Tree:"
            putStrLn (replicate 60 '-')
            putStrLn $ drawTree $ programToTree program
            exitSuccess

runMode ModePretty filePath content = do
    case runParser filePath content of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right program -> do
            putStrLn $ renderProgram program
            exitSuccess

runMode ModeTypeCheck filePath content = do
    case runParser filePath content of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right program -> do
            let result = typeCheck program
            case tcErrors result of
                [] -> do
                    putStrLn "Type checking passed! No semantic errors found."
                    putStrLn ""
                    putStrLn "Inferred function types:"
                    putStrLn (replicate 60 '-')
                    mapM_ printFuncType (Data.Map.Strict.toList (tcFuncTypes result))
                    exitSuccess
                errs -> do
                    hPutStrLn stderr $ "Found " ++ show (length errs) ++ " semantic error(s):"
                    hPutStrLn stderr (replicate 60 '-')
                    mapM_ (hPutStrLn stderr . prettyError) errs
                    exitFailure

runMode ModeInterpret filePath content = do
    -- Step 1: Parse
    case runParser filePath content of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right program -> do
            -- Step 2: Type check
            let tcResult = typeCheck program
            case tcErrors tcResult of
                [] -> do
                    -- Step 3: Interpret
                    result <- interpret program
                    case result of
                        Right _ -> exitSuccess
                        Left err -> do
                            hPutStrLn stderr $ prettyRuntimeError err
                            exitFailure
                errs -> do
                    hPutStrLn stderr $ "Found " ++ show (length errs) ++ " semantic error(s):"
                    hPutStrLn stderr (replicate 60 '-')
                    mapM_ (hPutStrLn stderr . prettyError) errs
                    exitFailure

-- | Print an inferred function type
printFuncType :: (T.Text, (Type, [Type])) -> IO ()
printFuncType (name, (retTy, paramTys)) = do
    putStrLn $ "  " ++ T.unpack name ++ "("
             ++ intercalate ", " (map showTy paramTys)
             ++ ") : " ++ showTy retTy
  where
    showTy TyInt    = "int"
    showTy TyFloat  = "float"
    showTy TyString = "string"
    showTy TyBool   = "bool"
    showTy TyVoid   = "void"
    showTy (TyVar v) = T.unpack v
    showTy (TyArray t _) = showTy t ++ "[]"
    showTy (TyStruct n) = T.unpack n
    showTy (TyFunc ps r) = "(" ++ intercalate ", " (map showTy ps) ++ ") -> " ++ showTy r

-- | Print a token with its position
printToken :: TokenPos -> IO ()
printToken (TokenPos (line, col) tok) = do
    let posStr = "[" ++ show line ++ ":" ++ show col ++ "]"
    let tokStr = formatToken tok
    putStrLn $ padRight 12 posStr ++ tokStr

-- | Pad string to the right
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- | Format a token for display
formatToken :: Token -> String
formatToken (TokInt n)       = "INT(" ++ show n ++ ")"
formatToken (TokFloat f)     = "FLOAT(" ++ show f ++ ")"
formatToken (TokString s)    = "STRING(\"" ++ T.unpack s ++ "\")"
formatToken (TokBool b)      = "BOOL(" ++ show b ++ ")"
formatToken (TokIdent s)     = "IDENT(" ++ T.unpack s ++ ")"
formatToken (TokReserved s)  = "RESERVED(" ++ T.unpack s ++ ")"
formatToken (TokOperator s)  = "OP(" ++ T.unpack s ++ ")"
formatToken TokLParen        = "LPAREN"
formatToken TokRParen        = "RPAREN"
formatToken TokLBrace        = "LBRACE"
formatToken TokRBrace        = "RBRACE"
formatToken TokLBracket      = "LBRACKET"
formatToken TokRBracket      = "RBRACKET"
formatToken TokSemi          = "SEMI"
formatToken TokComma         = "COMMA"
formatToken TokColon         = "COLON"
formatToken TokDot           = "DOT"
formatToken TokArrow         = "ARROW"
formatToken TokAssign        = "ASSIGN"
formatToken TokEOF           = "EOF"
