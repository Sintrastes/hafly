{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Hafly.Ast where
import Data.HashMap
import Data.Bits (Bits(xor))
import Data.Dynamic

newtype Program = Program (Map String Ast)

data Ast =
    Var String
  | App Ast Ast
  | Lambda [String] Ast
  | Sequence SequenceAst
  | Record (Map String Ast)
  | List [Ast]
  | String [StringSegment]
  | Cond Ast Ast Ast
  | Const Dynamic
        deriving(Show)

data StringSegment = 
    QuotedVar String 
  | StringSeq String 
  | QuotedExpr Ast
    deriving(Show)

subst :: String -> Ast -> Ast -> Ast
subst var x expr = case expr of
    Var v | var == v -> x
    App y z -> App 
        (subst var x y)
        (subst var x z)
    Cond y z w -> Cond
        (subst var x y)
        (subst var x z)
        (subst var x w)
    Record r -> Record $
        subst var x <$> r
    List xs -> List $
        subst var x <$> xs
    Lambda vars body -> Lambda vars
        (subst var x body)
    Sequence seq -> Sequence $ 
        substSeq var x seq 
    String xs -> String $ substString var x xs
    _ -> expr

substString :: String -> Ast -> [StringSegment] -> [StringSegment]
substString v a [] = []
substString v a (x:xs) = substStringSegment v a x : substString v a xs

substStringSegment :: String -> Ast -> StringSegment -> StringSegment
substStringSegment x a (QuotedVar y) | x == y = QuotedExpr a
substStringSegment x a (QuotedExpr y) = QuotedExpr $ subst x a y
substStringSegment x a y = y

substAll :: [(String, Ast)] -> Ast -> Ast
substAll [] x = x
substAll ((x,y):xs) a = substAll xs $ subst x y a

substSeq :: String -> Ast -> SequenceAst -> SequenceAst
substSeq var x (SequenceAst ss) = SequenceAst (fmap (substSeqExpr var x) ss)

substSeqExpr :: String -> Ast -> SequenceExpr -> SequenceExpr
substSeqExpr var x = \case
  Expr ast -> Expr $ subst var x ast
  BindExpr s ast -> BindExpr s $ 
      subst var x ast

data SequenceAst = SequenceAst [SequenceExpr]
    deriving(Show)

data SequenceExpr =
      Expr Ast
    | BindExpr String Ast
        deriving(Show)

type TypeError = String