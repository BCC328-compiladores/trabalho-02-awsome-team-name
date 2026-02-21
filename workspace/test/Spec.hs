{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.Text ()
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty)
import Data.Either (isRight, isLeft)
import qualified Data.Map.Strict as Map

import System.IO (hFlush, stdout)
import Data.IORef

import SL.AST
import SL.Lexer
import SL.Parser
import SL.Pretty
import SL.TypeChecker
import SL.Env
import SL.Interpreter

main :: IO ()
main = hspec $ do
    describe "Lexer" $ do
        lexerTests
    
    describe "Parser" $ do
        parserTests
    
    describe "Pretty Printer" $ do
        prettyPrinterTests
    
    describe "Negative Tests" $ do
        negativeTests
    
    describe "Integration Tests" $ do
        integrationTests
        moreIntegrationTests

    describe "TypeChecker - Valid Programs" $ do
        typeCheckerPositiveTests

    describe "TypeChecker - Invalid Programs" $ do
        typeCheckerNegativeTests

    describe "TypeChecker - Inference" $ do
        typeCheckerInferenceTests

    describe "TypeChecker - Integration" $ do
        typeCheckerIntegrationTests

    describe "Interpreter - Basic" $ do
        interpreterBasicTests

    describe "Interpreter - Control Flow" $ do
        interpreterControlFlowTests

    describe "Interpreter - Functions" $ do
        interpreterFunctionTests

    describe "Interpreter - Data Structures" $ do
        interpreterDataStructureTests

    describe "Interpreter - Integration" $ do
        interpreterIntegrationTests

    describe "Interpreter - Runtime Errors" $ do
        interpreterErrorTests

-- | Lexer tests
lexerTests :: Spec
lexerTests = do
    it "lexes integer literals" $ do
        let result = runLexer "<test>" "123"
        result `shouldSatisfy` isRight
        case result of
            Right tokens -> head tokens `shouldBe` TokenPos (1, 1) (TokInt 123)
            _ -> expectationFailure "Expected Right"
    
    it "lexes float literals" $ do
        let result = runLexer "<test>" "3.14"
        result `shouldSatisfy` isRight
        case result of
            Right tokens -> head tokens `shouldBe` TokenPos (1, 1) (TokFloat 3.14)
            _ -> expectationFailure "Expected Right"
    
    it "lexes string literals" $ do
        let result = runLexer "<test>" "\"hello\""
        result `shouldSatisfy` isRight
        case result of
            Right tokens -> head tokens `shouldBe` TokenPos (1, 1) (TokString "hello")
            _ -> expectationFailure "Expected Right"
    
    it "lexes boolean literals" $ do
        let resultTrue = runLexer "<test>" "true"
        let resultFalse = runLexer "<test>" "false"
        resultTrue `shouldSatisfy` isRight
        resultFalse `shouldSatisfy` isRight
    
    it "lexes identifiers" $ do
        let result = runLexer "<test>" "myVar"
        result `shouldSatisfy` isRight
        case result of
            Right tokens -> head tokens `shouldBe` TokenPos (1, 1) (TokIdent "myVar")
            _ -> expectationFailure "Expected Right"
    
    it "lexes reserved words" $ do
        let result = runLexer "<test>" "func struct if else while for return let new"
        result `shouldSatisfy` isRight
    
    it "lexes operators" $ do
        let result = runLexer "<test>" "+ - * / % == != < <= > >= && ||"
        result `shouldSatisfy` isRight
    
    it "lexes punctuation" $ do
        let result = runLexer "<test>" "( ) { } [ ] ; , : . ->"
        result `shouldSatisfy` isRight
    
    it "skips line comments" $ do
        let result = runLexer "<test>" "42 // this is a comment\n43"
        result `shouldSatisfy` isRight
        case result of
            Right tokens -> length (filter (not . isEOF) tokens) `shouldBe` 2
            _ -> expectationFailure "Expected Right"
      where
        isEOF (TokenPos _ TokEOF) = True
        isEOF _ = False

-- | Parser tests
parserTests :: Spec
parserTests = do
    it "parses integer literals" $ do
        let result = runParserExpr "<test>" "42"
        result `shouldSatisfy` isRight
        case result of
            Right expr -> expr `shouldBe` EInt 42
            _ -> expectationFailure "Expected Right"
    
    it "parses float literals" $ do
        let result = runParserExpr "<test>" "3.14"
        result `shouldSatisfy` isRight
        case result of
            Right expr -> expr `shouldBe` EFloat 3.14
            _ -> expectationFailure "Expected Right"
    
    it "parses string literals" $ do
        let result = runParserExpr "<test>" "\"hello world\""
        result `shouldSatisfy` isRight
    
    it "parses boolean literals" $ do
        let resultTrue = runParserExpr "<test>" "true"
        let resultFalse = runParserExpr "<test>" "false"
        resultTrue `shouldBe` Right (EBool True)
        resultFalse `shouldBe` Right (EBool False)
    
    it "parses variable references" $ do
        let result = runParserExpr "<test>" "myVar"
        result `shouldBe` Right (EVar "myVar")
    
    it "parses binary operations" $ do
        let result = runParserExpr "<test>" "1 + 2"
        result `shouldSatisfy` isRight
        case result of
            Right (EBinOp OpAdd (EInt 1) (EInt 2)) -> return ()
            Right other -> expectationFailure $ "Unexpected: " ++ show other
            _ -> expectationFailure "Expected Right"
    
    it "parses operator precedence correctly" $ do
        let result = runParserExpr "<test>" "1 + 2 * 3"
        result `shouldSatisfy` isRight
        case result of
            Right (EBinOp OpAdd (EInt 1) (EBinOp OpMul (EInt 2) (EInt 3))) -> return ()
            Right other -> expectationFailure $ "Unexpected: " ++ show other
            _ -> expectationFailure "Expected Right"
    
    it "parses function calls" $ do
        let result = runParserExpr "<test>" "foo(1, 2)"
        result `shouldSatisfy` isRight
    
    it "parses array indexing" $ do
        let result = runParserExpr "<test>" "arr[0]"
        result `shouldSatisfy` isRight
    
    it "parses field access" $ do
        let result = runParserExpr "<test>" "person.name"
        result `shouldSatisfy` isRight
    
    it "parses array literals" $ do
        let result = runParserExpr "<test>" "[1, 2, 3]"
        result `shouldSatisfy` isRight
    
    it "parses new expression" $ do
        let result = runParserExpr "<test>" "new int[10]"
        result `shouldSatisfy` isRight
    
    it "parses struct literals" $ do
        let result = runParserExpr "<test>" "Person{\"Alice\", 25}"
        result `shouldSatisfy` isRight
    
    it "parses unary operators" $ do
        let resultNeg = runParserExpr "<test>" "-5"
        let resultNot = runParserExpr "<test>" "!true"
        resultNeg `shouldSatisfy` isRight
        resultNot `shouldSatisfy` isRight
    
    it "parses simple function" $ do
        let src = "func main() : int { return 0; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses function with parameters" $ do
        let src = "func add(a : int, b : int) : int { return a + b; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses struct definition" $ do
        let src = "struct Person { name : string; age : int; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses let statement" $ do
        let src = "func test() : void { let x : int = 5; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses if-else statement" $ do
        let src = "func test() : void { if (x > 0) { return 1; } else { return 0; } }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses while loop" $ do
        let src = "func test() : void { while (i < 10) { i = i + 1; } }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses for loop" $ do
        let src = "func test() : void { for (i = 0; i < 10; i + 1) { print(i); } }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses forall (generics)" $ do
        let src = "forall a b . func map(f : (a) -> b, v : a[]) : b[] { return v; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses array types" $ do
        let src = "func test() : int[] { let arr : int[5]; return arr; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight

-- | Pretty printer tests
prettyPrinterTests :: Spec
prettyPrinterTests = do
    it "pretty prints integer literals" $ do
        let output = renderProgram (Program [TLFunc [] "main" [] (Just TyInt) [SReturn (Just (EInt 42))]])
        output `shouldContain` "42"
    
    it "pretty prints function definitions" $ do
        let output = renderProgram (Program [TLFunc [] "main" [] (Just TyInt) [SReturn (Just (EInt 0))]])
        output `shouldContain` "func"
        output `shouldContain` "main"
        output `shouldContain` "return"
    
    it "pretty prints struct definitions" $ do
        let output = renderProgram (Program [TLStruct "Person" [StructField "name" TyString]])
        output `shouldContain` "struct"
        output `shouldContain` "Person"
        output `shouldContain` "name"
        output `shouldContain` "string"
    
    it "pretty prints if-else statements" $ do
        let stmt = SIf (EBool True) [SReturn (Just (EInt 1))] (Just [SReturn (Just (EInt 0))])
        let output = renderProgram (Program [TLFunc [] "test" [] (Just TyInt) [stmt]])
        output `shouldContain` "if"
        output `shouldContain` "else"
    
    it "pretty prints while loops" $ do
        let stmt = SWhile (EBinOp OpLt (EVar "i") (EInt 10)) [SExpr (ECall "print" [EVar "i"])]
        let output = renderProgram (Program [TLFunc [] "test" [] (Just TyVoid) [stmt]])
        output `shouldContain` "while"
    
    it "pretty prints binary operations" $ do
        let expr = EBinOp OpAdd (EInt 1) (EInt 2)
        let output = renderProgram (Program [TLFunc [] "main" [] (Just TyInt) [SReturn (Just expr)]])
        output `shouldContain` "+"

-- | Integration tests with example programs
integrationTests :: Spec
integrationTests = do
    it "parses and pretty-prints factorial example" $ do
        let src = T.unlines
                [ "func factorial(n : int) : int {"
                , "    if (n <= 1) {"
                , "        return 1;"
                , "    } else {"
                , "        return n * factorial(n - 1);"
                , "    }"
                , "}"
                ]
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
        case result of
            Right prog -> do
                let output = renderProgram prog
                output `shouldContain` "factorial"
                output `shouldContain` "return"
            _ -> expectationFailure "Expected Right"
    
    it "parses struct with fields" $ do
        let src = T.unlines
                [ "struct Person {"
                , "    name : string;"
                , "    age : int;"
                , "    height : float;"
                , "}"
                ]
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
    
    it "parses function with generic type" $ do
        let src = "forall a b . func map(f : (a) -> b, v : a[]) : b[] { return v; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
        case result of
            Right (Program [TLFunc tvs name _ _ _]) -> do
                tvs `shouldBe` ["a", "b"]
                name `shouldBe` "map"
            _ -> expectationFailure "Expected generic function"
    
    it "round-trips simple program" $ do
        let src = "func main() : int { return 0; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isRight
        case result of
            Right prog -> do
                let output = renderProgram prog
                -- Parse the output again
                let result2 = runParser "<test>" (T.pack output)
                result2 `shouldSatisfy` isRight
            _ -> expectationFailure "Expected Right"

-- | Negative tests (Error cases)
negativeTests :: Spec
negativeTests = do
    it "fails on keyword used as identifier" $ do
        let result = runLexer "<test>" "func if() : void {}"
        -- This parsing is tricky because "func" consumes, then "if" is parsed as Reserved.
        -- The parser expects an identifier.
        let resultParse = runParser "<test>" "func if() : void {}"
        resultParse `shouldSatisfy` isLeft

    it "fails on invalid identifier starting with number" $ do
        -- "1var" lexes as Int(1) then Ident(var). Syntactically invalid in most places.
        let resultParse = runParserExpr "<test>" "1var"
        -- Parser expects an expression. Int(1) is an expr. Then 'var' remains.
        -- runParserExpr expects EOF. So this should fail because not all input is consumed.
        resultParse `shouldSatisfy` isLeft

    it "fails on missing semicolon in struct" $ do
        let src = "struct Point { x : int y : int; }"
        let result = runParser "<test>" src
        result `shouldSatisfy` isLeft

    it "fails on missing closing brace" $ do
        let src = "func main() : void { return;"
        let result = runParser "<test>" src
        result `shouldSatisfy` isLeft

    it "fails on malformed type" $ do
        -- 'int[' is invalid
        let src = "func main(x : int[) : void {}" 
        runParser "<test>" src `shouldSatisfy` isLeft

-- | Additional Integration Tests
moreIntegrationTests :: Spec
moreIntegrationTests = do
    it "parses Exemplo 2: Records and Arrays" $ do
        let src = T.unlines
                [ "struct Person {"
                , "    name : string;"
                , "    age : int;"
                , "    height : float;"
                , "}"
                , ""
                , "func main() : void {"
                , "    let people : People[3];"
                , "    people[0] = Person{\"Alice\", 25, 1.65};"
                , "    people[1] = Person{\"Bob\", 30, 1.80};"
                , "    people[2] = Person{\"Charlie\", 35, 1.75};"
                , "    let i : int = 0;"
                , "    while (i < 3) {"
                , "        print(people[i].name);"
                , "        print(people[i].age);"
                , "        print(people[i].height);"
                , "        i = i + 1;"
                , "    }"
                , "}"
                ]
        runParser "<test>" src `shouldSatisfy` isRight

    it "parses Exemplo 3: Array Operations" $ do
        let src = T.unlines
                [ "func reverse(arr : int[], size : int) : int [] {"
                , "    let result : int[] = new int[size];"
                , "    let i : int = 0;"
                , "    while (i < size) {"
                , "        result[i] = arr[size - i - 1];"
                , "        i = i + 1;"
                , "    }"
                , "    return result;"
                , "}"
                , ""
                , "func main() : void {"
                , "    let original : int[5] = [1, 2, 3, 4, 5];"
                , "    let reversed : int[] = reverse(original, 5);"
                , "    let j : int = 0;"
                , "    while (j < 5) {"
                , "        print(reversed[j]);"
                , "        j = j + 1;"
                , "    }"
                , "}"
                ]
        runParser "<test>" src `shouldSatisfy` isRight

    it "parses Exemplo 4: Math and Bool" $ do
        let src = T.unlines
                [ "func calculateBMI(weight : float, height : float) : float {"
                , "    return weight / (height * height);"
                , "}"
                , ""
                , "func isAdult(age : int) : bool {"
                , "    return age >= 18;"
                , "}"
                , ""
                , "func main() : void {"
                , "    let bmi : float = calculateBMI(70.5, 1.75);"
                , "    let adult : bool = isAdult(20);"
                , "    print(bmi);"
                , "    print(adult);"
                , "    if (adult && bmi > 25.0) {"
                , "        print(\"Adulto com sobrepeso\");"
                , "    } else {"
                , "        print(\"Condição normal\");"
                , "    }"
                , "}"
                ]
        runParser "<test>" src `shouldSatisfy` isRight

    it "matches precedent for complex expression" $ do
        -- 1 + 2 * 3 should be 1 + (2 * 3)
        let result = runParserExpr "<test>" "1 + 2 * 3"
        case result of
             Right (EBinOp OpAdd (EInt 1) (EBinOp OpMul (EInt 2) (EInt 3))) -> return ()
             Right other -> expectationFailure $ "Precedence error: 1 + 2 * 3, got " ++ show other
             Left err -> expectationFailure $ "Parse error: " ++ errorBundlePretty err

        -- (1 + 2) * 3
        let result2 = runParserExpr "<test>" "(1 + 2) * 3"
        case result2 of
             Right (EBinOp OpMul (EBinOp OpAdd (EInt 1) (EInt 2)) (EInt 3)) -> return ()
             Right other -> expectationFailure $ "Precedence error: (1 + 2) * 3, got " ++ show other
             Left err -> expectationFailure $ "Parse error: " ++ errorBundlePretty err

-- ============================================================
-- Helper for type checker tests
-- ============================================================

-- | Parse and type-check source code, returning the TCResult
typeCheckSrc :: T.Text -> TCResult
typeCheckSrc src =
    case runParser "<test>" src of
        Left err -> error $ "Parse error in test: " ++ errorBundlePretty err
        Right prog -> typeCheck prog

-- | Assert that source code type-checks without errors
shouldTypeCheck :: T.Text -> Expectation
shouldTypeCheck src =
    let result = typeCheckSrc src
    in case tcErrors result of
        [] -> return ()
        errs -> expectationFailure $ "Expected no errors, got:\n"
              ++ unlines (map prettyError errs)

-- | Assert that source code produces an error containing the given substring
shouldFailWith :: T.Text -> T.Text -> Expectation
shouldFailWith src substr =
    let result = typeCheckSrc src
        msgs = map errMessage (tcErrors result)
    in if any (T.isInfixOf substr) msgs
       then return ()
       else expectationFailure $ "Expected error containing '"
              ++ T.unpack substr ++ "', got:\n"
              ++ unlines (map prettyError (tcErrors result))

-- ============================================================
-- Type Checker: Positive tests (valid programs)
-- ============================================================

typeCheckerPositiveTests :: Spec
typeCheckerPositiveTests = do
    it "accepts simple function with correct types" $ do
        shouldTypeCheck "func main() : int { return 0; }"

    it "accepts function with typed parameters" $ do
        shouldTypeCheck "func add(a : int, b : int) : int { return a + b; }"

    it "accepts let with type annotation and value" $ do
        shouldTypeCheck "func test() : void { let x : int = 5; }"

    it "accepts let with inferred type from value" $ do
        shouldTypeCheck "func test() : void { let x = 42; print(x); }"

    it "accepts let without value (declaration only)" $ do
        shouldTypeCheck "func test() : void { let x : int; }"

    it "accepts arithmetic with int operands" $ do
        shouldTypeCheck "func test() : int { return 1 + 2 * 3; }"

    it "accepts arithmetic with float operands" $ do
        shouldTypeCheck "func test() : float { return 1.0 + 2.5; }"

    it "accepts mixed int/float arithmetic" $ do
        shouldTypeCheck "func test() : float { return 1 + 2.5; }"

    it "accepts comparison operators" $ do
        shouldTypeCheck "func test() : bool { return 1 < 2; }"

    it "accepts logical operators" $ do
        shouldTypeCheck "func test() : bool { return true && false; }"

    it "accepts if with bool condition" $ do
        shouldTypeCheck $ T.unlines
            [ "func test() : int {"
            , "    if (true) { return 1; } else { return 0; }"
            , "}"
            ]

    it "accepts while with bool condition" $ do
        shouldTypeCheck $ T.unlines
            [ "func test() : void {"
            , "    let i : int = 0;"
            , "    while (i < 10) { i = i + 1; }"
            , "}"
            ]

    it "accepts for loop" $ do
        shouldTypeCheck $ T.unlines
            [ "func test() : void {"
            , "    for (i = 0; i < 10; i + 1) { print(i); }"
            , "}"
            ]

    it "accepts print as builtin" $ do
        shouldTypeCheck "func test() : void { print(42); }"

    it "accepts function calling another function" $ do
        shouldTypeCheck $ T.unlines
            [ "func double(x : int) : int { return x * 2; }"
            , "func main() : int { return double(5); }"
            ]

    it "accepts recursive function" $ do
        shouldTypeCheck $ T.unlines
            [ "func factorial(n : int) : int {"
            , "    if (n <= 1) { return 1; }"
            , "    else { return n * factorial(n - 1); }"
            , "}"
            ]

    it "accepts struct construction" $ do
        shouldTypeCheck $ T.unlines
            [ "struct Point { x : int; y : int; }"
            , "func test() : void { let p = Point{1, 2}; }"
            ]

    it "accepts array operations" $ do
        shouldTypeCheck $ T.unlines
            [ "func test() : void {"
            , "    let arr : int[] = [1, 2, 3];"
            , "    let x : int = arr[0];"
            , "}"
            ]

    it "accepts new array" $ do
        shouldTypeCheck $ T.unlines
            [ "func test() : void {"
            , "    let arr : int[] = new int[10];"
            , "}"
            ]

    it "accepts void return" $ do
        shouldTypeCheck "func test() : void { return; }"

    it "accepts unary negation" $ do
        shouldTypeCheck "func test() : int { return -5; }"

    it "accepts logical not" $ do
        shouldTypeCheck "func test() : bool { return !true; }"

    it "accepts equality operators" $ do
        shouldTypeCheck "func test() : bool { return 1 == 2; }"

    it "accepts modulo operator" $ do
        shouldTypeCheck "func test() : int { return 10 % 3; }"

-- ============================================================
-- Type Checker: Negative tests (invalid programs)
-- ============================================================

typeCheckerNegativeTests :: Spec
typeCheckerNegativeTests = do
    it "rejects undefined variable" $ do
        shouldFailWith
            "func test() : int { return x; }"
            "Undefined variable"

    it "rejects undefined function call" $ do
        shouldFailWith
            "func test() : int { return foo(1); }"
            "Undefined function"

    it "rejects arithmetic on string" $ do
        shouldFailWith
            "func test() : int { return \"hello\" + 1; }"
            "requires numeric"

    it "rejects arithmetic on bool" $ do
        shouldFailWith
            "func test() : int { return true + 1; }"
            "requires numeric"

    it "rejects logical and on int" $ do
        shouldFailWith
            "func test() : bool { return 1 && 2; }"
            "requires bool"

    it "rejects if with non-bool condition" $ do
        shouldFailWith
            "func test() : void { if (42) { return; } }"
            "if condition must be bool"

    it "rejects while with non-bool condition" $ do
        shouldFailWith
            "func test() : void { while (\"yes\") { return; } }"
            "while condition must be bool"

    it "rejects wrong argument count" $ do
        shouldFailWith
            (T.unlines
                [ "func add(a : int, b : int) : int { return a + b; }"
                , "func test() : int { return add(1); }"
                ])
            "expects 2 arguments but got 1"

    it "rejects wrong argument type" $ do
        shouldFailWith
            (T.unlines
                [ "func square(x : int) : int { return x * x; }"
                , "func test() : int { return square(\"hello\"); }"
                ])
            "Type mismatch"

    it "rejects return type mismatch" $ do
        shouldFailWith
            "func test() : int { return \"hello\"; }"
            "Type mismatch"

    it "rejects empty return in non-void function" $ do
        shouldFailWith
            "func test() : int { return; }"
            "Expected return value"

    it "rejects negation of string" $ do
        shouldFailWith
            "func test() : int { let s : string = \"x\"; return -s; }"
            "requires numeric"

    it "rejects logical not on int" $ do
        shouldFailWith
            "func test() : bool { return !42; }"
            "requires bool"

    it "rejects indexing non-array" $ do
        shouldFailWith
            "func test() : int { let x : int = 5; return x[0]; }"
            "Cannot index non-array"

    it "rejects duplicate function definition" $ do
        shouldFailWith
            (T.unlines
                [ "func foo() : void {}"
                , "func foo() : int { return 1; }"
                ])
            "already defined"

    it "rejects duplicate struct definition" $ do
        shouldFailWith
            (T.unlines
                [ "struct Point { x : int; }"
                , "struct Point { y : float; }"
                ])
            "already defined"

    it "rejects struct with wrong field count" $ do
        shouldFailWith
            (T.unlines
                [ "struct Point { x : int; y : int; }"
                , "func test() : void { let p = Point{1}; }"
                ])
            "has 2 fields but got 1"

    it "rejects incompatible assignment" $ do
        shouldFailWith
            (T.unlines
                [ "func test() : void {"
                , "    let x : int = 5;"
                , "    x = \"hello\";"
                , "}"
                ])
            "Type mismatch"

    it "rejects array index with non-int" $ do
        shouldFailWith
            (T.unlines
                [ "func test() : void {"
                , "    let arr : int[] = [1, 2, 3];"
                , "    let x : int = arr[\"zero\"];"
                , "}"
                ])
            "array index must be int"

    it "rejects new with non-int size" $ do
        shouldFailWith
            "func test() : void { let arr : int[] = new int[\"ten\"]; }"
            "new array size must be int"

    it "rejects duplicate variable declaration in same scope" $ do
        shouldFailWith
            (T.unlines
                [ "func test() : void {"
                , "    let x : int = 5;"
                , "    let x : int = 10;"
                , "}"
                ])
            "already declared"

-- ============================================================
-- Type Checker: Inference tests
-- ============================================================

typeCheckerInferenceTests :: Spec
typeCheckerInferenceTests = do
    it "infers type of let from integer literal" $ do
        shouldTypeCheck "func test() : void { let x = 42; print(x); }"

    it "infers type of let from string literal" $ do
        shouldTypeCheck "func test() : void { let x = \"hello\"; print(x); }"

    it "infers type of let from expression" $ do
        shouldTypeCheck "func test() : void { let x = 1 + 2; print(x); }"

    it "accepts function without return type (inferred)" $ do
        shouldTypeCheck "func test() { return 42; }"

    it "accepts identity function without types (Example 5)" $ do
        shouldTypeCheck "func id(x) { return x; }"

    it "accepts generic function with forall (Example 6)" $ do
        shouldTypeCheck $ T.unlines
            [ "forall a b . func map(f : (a) -> b, v : a[]) : b[] {"
            , "    let result = new b[v.size];"
            , "    for (i = 0; i < v.size; i + 1) {"
            , "        result[i] = f(v[i]);"
            , "    }"
            , "    return result;"
            , "}"
            ]

    it "records inferred function types" $ do
        let result = typeCheckSrc "func add(a : int, b : int) : int { return a + b; }"
        tcErrors result `shouldBe` []
        Map.member "add" (tcFuncTypes result) `shouldBe` True

    it "accepts array .size property" $ do
        shouldTypeCheck $ T.unlines
            [ "func test() : void {"
            , "    let arr : int[] = [1, 2, 3];"
            , "    let n : int = arr.size;"
            , "}"
            ]

-- ============================================================
-- Type Checker: Integration with example programs
-- ============================================================

typeCheckerIntegrationTests :: Spec
typeCheckerIntegrationTests = do
    it "type-checks Example 1: factorial" $ do
        shouldTypeCheck $ T.unlines
            [ "func factorial(n : int) : int {"
            , "    if (n <= 1) {"
            , "        return 1;"
            , "    } else {"
            , "        return n * factorial(n - 1);"
            , "    }"
            , "}"
            , ""
            , "func main() : int {"
            , "    let result : int = factorial(5);"
            , "    print(result);"
            , "    return 1;"
            , "}"
            ]

    it "type-checks Example 3: array operations" $ do
        shouldTypeCheck $ T.unlines
            [ "func reverse(arr : int[], size : int) : int[] {"
            , "    let result : int[] = new int[size];"
            , "    let i : int = 0;"
            , "    while (i < size) {"
            , "        result[i] = arr[size - i - 1];"
            , "        i = i + 1;"
            , "    }"
            , "    return result;"
            , "}"
            , ""
            , "func main() : void {"
            , "    let original : int[5] = [1, 2, 3, 4, 5];"
            , "    let reversed : int[] = reverse(original, 5);"
            , "    let j : int = 0;"
            , "    while (j < 5) {"
            , "        print(reversed[j]);"
            , "        j = j + 1;"
            , "    }"
            , "}"
            ]

    it "type-checks Example 4: math and bool" $ do
        shouldTypeCheck $ T.unlines
            [ "func calculateBMI(weight : float, height : float) : float {"
            , "    return weight / (height * height);"
            , "}"
            , ""
            , "func isAdult(age : int) : bool {"
            , "    return age >= 18;"
            , "}"
            , ""
            , "func main() : void {"
            , "    let bmi : float = calculateBMI(70.5, 1.75);"
            , "    let adult : bool = isAdult(20);"
            , "    print(bmi);"
            , "    print(adult);"
            , "    if (adult && bmi > 25.0) {"
            , "        print(\"Adulto com sobrepeso\");"
            , "    } else {"
            , "        print(\"Condicao normal\");"
            , "    }"
            , "}"
            ]

    it "type-checks Example 5: identity (inference)" $ do
        shouldTypeCheck "func id(x) { return x; }"

    it "type-checks Example 6: generics" $ do
        shouldTypeCheck $ T.unlines
            [ "forall a b . func map(f : (a) -> b, v : a[]) : b[] {"
            , "    let result = new b[v.size];"
            , "    for (i = 0; i < v.size; i + 1) {"
            , "        result[i] = f(v[i]);"
            , "    }"
            , "    return result;"
            , "}"
            ]

    it "type-checks struct with field access" $ do
        shouldTypeCheck $ T.unlines
            [ "struct Person {"
            , "    name : string;"
            , "    age : int;"
            , "    height : float;"
            , "}"
            , ""
            , "func test() : void {"
            , "    let p = Person{\"Alice\", 25, 1.65};"
            , "    print(p.name);"
            , "    print(p.age);"
            , "}"
            ]

    it "type-checks multiple functions" $ do
        shouldTypeCheck $ T.unlines
            [ "func square(x : int) : int { return x * x; }"
            , "func sumOfSquares(a : int, b : int) : int {"
            , "    return square(a) + square(b);"
            , "}"
            , "func main() : void {"
            , "    let result : int = sumOfSquares(3, 4);"
            , "    print(result);"
            , "}"
            ]

    it "detects multiple errors in one program" $ do
        let result = typeCheckSrc $ T.unlines
                [ "func test() : void {"
                , "    let x : int = \"hello\";"
                , "    let y : bool = 42 + true;"
                , "}"
                ]
        length (tcErrors result) `shouldSatisfy` (>= 2)

-- ============================================================
-- Interpreter test helpers
-- ============================================================

-- | Parse and interpret source code, returning Either RuntimeError Value
interpretSrc :: T.Text -> IO (Either RuntimeError Value)
interpretSrc src =
    case runParser "<test>" src of
        Left err -> error $ "Parse error in test: " ++ errorBundlePretty err
        Right prog -> interpret prog

-- | Assert that source code runs without error
shouldRunOk :: T.Text -> Expectation
shouldRunOk src = do
    result <- interpretSrc src
    case result of
        Right _ -> return ()
        Left err -> expectationFailure $ prettyRuntimeError err

-- | Assert that source code produces a runtime error
shouldRunError :: T.Text -> T.Text -> Expectation
shouldRunError src substr = do
    result <- interpretSrc src
    case result of
        Left err ->
            if T.isInfixOf substr (rteMessage err)
            then return ()
            else expectationFailure $ "Expected error containing '"
                   ++ T.unpack substr ++ "', got: " ++ prettyRuntimeError err
        Right _ -> expectationFailure "Expected runtime error, but program succeeded"

-- ============================================================
-- Interpreter: Basic tests
-- ============================================================

interpreterBasicTests :: Spec
interpreterBasicTests = do
    it "runs a simple main function" $ do
        shouldRunOk "func main() : int { return 0; }"

    it "runs main returning void" $ do
        shouldRunOk "func main() : void { return; }"

    it "evaluates integer arithmetic" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let x : int = 2 + 3 * 4;"
            , "    print(x);"
            , "}"
            ]

    it "evaluates float arithmetic" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let x : float = 1.5 + 2.5;"
            , "    print(x);"
            , "}"
            ]

    it "evaluates boolean operations" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let a : bool = true && false;"
            , "    let b : bool = true || false;"
            , "    let c : bool = !true;"
            , "    print(a);"
            , "    print(b);"
            , "    print(c);"
            , "}"
            ]

    it "evaluates comparison operators" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    print(1 < 2);"
            , "    print(3 >= 3);"
            , "    print(5 == 5);"
            , "    print(1 != 2);"
            , "}"
            ]

    it "evaluates unary negation" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let x : int = -5;"
            , "    print(x);"
            , "}"
            ]

    it "evaluates string literals" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let s : string = \"Hello World\";"
            , "    print(s);"
            , "}"
            ]

    it "evaluates modulo" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let x : int = 10 % 3;"
            , "    print(x);"
            , "}"
            ]

-- ============================================================
-- Interpreter: Control flow tests
-- ============================================================

interpreterControlFlowTests :: Spec
interpreterControlFlowTests = do
    it "executes if-true branch" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    if (true) { print(1); } else { print(2); }"
            , "}"
            ]

    it "executes if-false (else) branch" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    if (false) { print(1); } else { print(2); }"
            , "}"
            ]

    it "executes while loop" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let i : int = 0;"
            , "    while (i < 5) {"
            , "        print(i);"
            , "        i = i + 1;"
            , "    }"
            , "}"
            ]

    it "executes for loop" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    for (i = 0; i < 5; i + 1) {"
            , "        print(i);"
            , "    }"
            , "}"
            ]

    it "handles nested if-else" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let x : int = 10;"
            , "    if (x > 5) {"
            , "        if (x > 15) { print(1); } else { print(2); }"
            , "    } else {"
            , "        print(3);"
            , "    }"
            , "}"
            ]

-- ============================================================
-- Interpreter: Function tests
-- ============================================================

interpreterFunctionTests :: Spec
interpreterFunctionTests = do
    it "calls a simple function" $ do
        shouldRunOk $ T.unlines
            [ "func double(x : int) : int { return x * 2; }"
            , "func main() : void {"
            , "    let result : int = double(5);"
            , "    print(result);"
            , "}"
            ]

    it "supports recursive functions" $ do
        shouldRunOk $ T.unlines
            [ "func factorial(n : int) : int {"
            , "    if (n <= 1) { return 1; }"
            , "    else { return n * factorial(n - 1); }"
            , "}"
            , "func main() : void {"
            , "    print(factorial(5));"
            , "}"
            ]

    it "supports multiple parameters" $ do
        shouldRunOk $ T.unlines
            [ "func add(a : int, b : int) : int { return a + b; }"
            , "func main() : void { print(add(3, 4)); }"
            ]

    it "supports function calling function" $ do
        shouldRunOk $ T.unlines
            [ "func square(x : int) : int { return x * x; }"
            , "func sumOfSquares(a : int, b : int) : int {"
            , "    return square(a) + square(b);"
            , "}"
            , "func main() : void { print(sumOfSquares(3, 4)); }"
            ]

-- ============================================================
-- Interpreter: Data structure tests
-- ============================================================

interpreterDataStructureTests :: Spec
interpreterDataStructureTests = do
    it "creates and accesses arrays" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let arr : int[] = [10, 20, 30];"
            , "    print(arr[0]);"
            , "    print(arr[1]);"
            , "    print(arr[2]);"
            , "}"
            ]

    it "creates arrays with new" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let arr : int[] = new int[3];"
            , "    arr[0] = 100;"
            , "    arr[1] = 200;"
            , "    arr[2] = 300;"
            , "    print(arr[0]);"
            , "    print(arr[2]);"
            , "}"
            ]

    it "supports array .size" $ do
        shouldRunOk $ T.unlines
            [ "func main() : void {"
            , "    let arr : int[] = [1, 2, 3, 4, 5];"
            , "    print(arr.size);"
            , "}"
            ]

    it "creates and accesses structs" $ do
        shouldRunOk $ T.unlines
            [ "struct Point { x : int; y : int; }"
            , "func main() : void {"
            , "    let p = Point{10, 20};"
            , "    print(p.x);"
            , "    print(p.y);"
            , "}"
            ]

    it "supports array of structs" $ do
        shouldRunOk $ T.unlines
            [ "struct Person { name : string; age : int; }"
            , "func main() : void {"
            , "    let people : Person[2];"
            , "    people[0] = Person{\"Alice\", 25};"
            , "    people[1] = Person{\"Bob\", 30};"
            , "    print(people[0].name);"
            , "    print(people[1].age);"
            , "}"
            ]

-- ============================================================
-- Interpreter: Integration tests with example programs
-- ============================================================

interpreterIntegrationTests :: Spec
interpreterIntegrationTests = do
    it "runs Example 1: factorial" $ do
        src <- TIO.readFile "test/inputs/example1_factorial.sl"
        result <- interpret =<< parseOrFail src
        result `shouldSatisfy` isRight'

    it "runs Example 2: structs and arrays" $ do
        src <- TIO.readFile "test/inputs/example2_structs.sl"
        result <- interpret =<< parseOrFail src
        result `shouldSatisfy` isRight'

    it "runs Example 3: array operations (reverse)" $ do
        src <- TIO.readFile "test/inputs/example3_arrays.sl"
        result <- interpret =<< parseOrFail src
        result `shouldSatisfy` isRight'

    it "runs Example 4: math and boolean ops" $ do
        src <- TIO.readFile "test/inputs/example4_math.sl"
        result <- interpret =<< parseOrFail src
        result `shouldSatisfy` isRight'

-- ============================================================
-- Interpreter: Runtime error tests
-- ============================================================

interpreterErrorTests :: Spec
interpreterErrorTests = do
    it "reports error on missing main" $ do
        shouldRunError
            "func foo() : void {}"
            "No 'main' function"

    it "reports error on division by zero" $ do
        shouldRunError
            (T.unlines
                [ "func main() : void {"
                , "    let x : int = 10 / 0;"
                , "}"
                ])
            "Division by zero"

    it "reports error on array out of bounds" $ do
        shouldRunError
            (T.unlines
                [ "func main() : void {"
                , "    let arr : int[] = [1, 2, 3];"
                , "    print(arr[5]);"
                , "}"
                ])
            "out of bounds"

    it "reports error on undefined variable" $ do
        shouldRunError
            "func main() : void { print(x); }"
            "Undefined variable"

    it "reports error on undefined function call" $ do
        shouldRunError
            "func main() : void { foo(); }"
            "Undefined function"

-- ============================================================
-- Interpreter helper
-- ============================================================

parseOrFail :: T.Text -> IO Program
parseOrFail src =
    case runParser "<test>" src of
        Left err -> error $ "Parse error: " ++ errorBundlePretty err
        Right prog -> return prog

isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _         = False
