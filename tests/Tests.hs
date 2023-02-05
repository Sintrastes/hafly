
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Language.Hafly.Parser
import Language.Hafly.Ast

main = defaultMain $ testGroup "Hafly Test Suite" 
    [
      testCase "Parse string literal" $ do
        let string = "\"test\""
        let result = parseExpression [] string

        let expected = Right $ String [StringSeq "test"]
        
        result @=? expected
    , testCase "Parse string literal with simple quote 1" $ do
        return ()
    , testCase "Parse string literal with simple quote 2" $ do
        return ()
    -- Currently this would fail
    , testCase "Parse string literal with complex quote" $ do
        return ()
    , testCase "Parse lambda expression" $ do
        return ()
    , testCase "Parse if-else expression" $ do
        return ()
    ]