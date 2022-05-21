
module Language.Hafly.Ast where
import Data.HashMap

data Ast =
    Atom String
  | Literal LiteralExpr
  | App Ast Ast
  -- I'm not sure if this is nescesarry or not with Dynamic.
  | Lambda (Ast -> Ast)
  | Sequence SequenceAst
  | Var String

data LiteralExpr =
    IntLit Int
  | DoubleLit Double
  | StringLit String
  | Record (Map String LiteralExpr)

data SequenceAst = SequenceAst [SequenceExpr]

data SequenceExpr =
      Expr Ast
    | BindExpr String Ast

type TypeError = String