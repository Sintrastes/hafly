
module Language.Hafly.Parser where
import Language.Hafly.Ast (LiteralExpr, SequenceAst, Ast)
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T

type Parser = Parsec Void T.Text

-- Parser for an entire hafly program.
program :: Parser Program
program = undefined 

-- | Parse an arbitrary hafly expression.
expr :: Parser Ast
expr = undefined

-- Parse a literal expression
literal :: Parser LiteralExpr
literal = undefined

-- Parse a variable
-- Note: Will need to figure out from context which of these are bound/unbound.
var :: Ast
var = undefined

-- Parse a sequntial block.
block :: Parser SequenceAst
block = undefined

-- Parse a function application.
app :: Ast
app = undefined

-- Parse an expression with operators.
opExpr :: Ast 
opExpr = undefined