{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Hafly.Ast where
import Data.HashMap

newtype Program = Program (Map String Ast)

data Ast =
    Atom String
  | Literal LiteralExpr
  | App Ast Ast
  -- I'm not sure if this is nescesarry or not with Dynamic.
  -- | Lambda (Ast -> Ast)
  | Sequence SequenceAst
  | Var String 
        deriving(Show)

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