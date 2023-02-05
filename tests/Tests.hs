
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Language.Hafly.Parser
import Language.Hafly.Ast
import Language.Hafly.Stdlib
import Language.Hafly.Interpreter

main = defaultMain $ testGroup "Hafly Test Suite" 
    [
      testCase "Parse string literal" $ do
        let string = "\"test\""
        let result = parseExpression [] string

        let expected = Right $ String [StringSeq "test"]
        
        expected @=? result
    , testCase "Parse string literal with simple quote 1" $ do
        let string = "\"Hello $name\""
        let result = parseExpression [] string

        let expected = Right $ String [StringSeq "Hello ", QuotedExpr $ Var "name"]

        expected @=? result
    , testCase "Parse string literal with simple quote 2" $ do
        let string = "\"Hello ${name}\""
        let result = parseExpression [] string

        let expected = Right $ String [StringSeq "Hello ", QuotedExpr $ Var "name"]

        expected @=? result
    , testCase "Parse string literal with complex quote" $ do
        let string = "\"Test ${x + x}\""
        let result = parseExpression (operatorDefs base) string

        let expected = Right $ String [StringSeq "Test ", QuotedExpr $ Var "+" `App` Var "x" `App` Var "x"]

        expected @=? result
    , testCase "Parse lambda expression" $ do
        let string = "\\x y -> x"
        let result = parseExpression [] string

        let expected = Right $ Lambda ["x", "y"] (Var "x")

        expected @=? result
    , testCase "Parse if-else expression" $ do
        let string = "if (test) x else y"
        let result = parseExpression [] string

        let expected = Right $ Cond (Var "test") (Var "x") (Var "y")

        expected @=? result
    ]