{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Hafly.Ast where
import Data.HashMap
import Data.Bits (Bits(xor))
import Data.Dynamic

newtype Program = Program (Map String Ast)

data Ast =
    Atom String
  | Literal LiteralExpr
  | App Ast Ast
  | Lambda [String] Ast
  | Sequence SequenceAst
  | Var String 
  | Const Dynamic
        deriving(Show)

subst :: String -> Ast -> Ast -> Ast
subst var x expr = case expr of
    Var v | var == v -> x
    Atom v | var == v -> x
    App y z -> App 
        (subst var x y)
        (subst var x z)
    Lambda vars body -> Lambda vars
        (subst var x body)
    Sequence seq -> Sequence $ 
        substSeq var x seq 
    _ -> expr

substSeq :: String -> Ast -> SequenceAst -> SequenceAst
substSeq var x (SequenceAst ss) = SequenceAst (fmap (substSeqExpr var x) ss)

substSeqExpr :: String -> Ast -> SequenceExpr -> SequenceExpr
substSeqExpr var x = \case
  Expr ast -> Expr $ subst var x ast
  BindExpr s ast -> BindExpr s $ 
      subst var x ast

data LiteralExpr =
    IntLit Int
  | DoubleLit Double
  | StringLit String
  | Record (Map String LiteralExpr)
        deriving(Show)

data SequenceAst = SequenceAst [SequenceExpr]
    deriving(Show)

data SequenceExpr =
      Expr Ast
    | BindExpr String Ast
        deriving(Show)

type TypeError = String