
module Language.Hafly.Parser(
    parseExpression,
    parseProgram
) where
import Language.Hafly.Ast
import Text.Megaparsec hiding (token)
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.HashMap (fromList)

type Parser = Parsec Void T.Text

parseExpression :: T.Text -> Either (ParseErrorBundle T.Text Void) Ast
parseExpression = parse (expr <* eof) ""

parseProgram :: T.Text -> Either (ParseErrorBundle T.Text Void) Program
parseProgram = parse program ""

whitespace :: Parser ()
whitespace = do
    many $
        try (char ' ' >> pure ()) <|>
            (char '\n' >> pure ())
    pure ()

token p = do
    whitespace
    res <- p
    whitespace
    pure res

leftBracket = token (char '{')

rightBracket = token (char '}')

leftParen = token (char '(')

rightParen = token (char ')')

bindToken = token (string "<-")

arrToken = token (string "->")

lambdaToken = token (char '\\')

semicolon = token (char ';')

equals = token (char '=')

identifier = token (some alphaNumChar)

inParens p = do
    leftParen
    res <- p
    rightParen
    pure res

-- Parser for an entire hafly program.
program :: Parser Program
program = Program . fromList <$> 
    sepBy exprDef (token newline)

exprDef :: Parser (String, Ast)
exprDef = do
    x <- identifier
    equals
    y <- expr
    return (x, y)

-- | Parse an arbitrary hafly expression.
expr :: Parser Ast
expr = try app
  <|> baseExpr

baseExpr :: Parser Ast
baseExpr = try (Literal <$> literal)
    <|> try lambdaExpr
    <|> try (Atom <$> identifier)
    <|> (Sequence <$> block)

lambdaExpr :: Parser Ast
lambdaExpr = do
    lambdaToken
    vars <- sepBy identifier whitespace
    arrToken
    body <- expr
    return $ Lambda vars body

-- Parse a literal expression
literal :: Parser LiteralExpr
literal = try intLit <|>
    stringLit

intLit = token $ 
    IntLit . read <$> some digitChar

stringLit = token $ do
    char '"'
    x <- many $ noneOf ['"']
    char '"'
    return $ StringLit x

-- Parse a sequntial block.
block :: Parser SequenceAst
block = do
    leftBracket
    res <- sepBy1 sequenceExpr (token semicolon)
    rightBracket
    return $ SequenceAst res

sequenceExpr :: Parser SequenceExpr
sequenceExpr = try bindExpr 
    <|> Expr <$> expr

bindExpr :: Parser SequenceExpr
bindExpr = do
    x <- identifier
    bindToken
    y <- expr
    return $ BindExpr x y

-- Parse a function application.
app :: Parser Ast
app = foldl1 App <$>
    some (try baseExpr <|> inParens expr)

-- Parse an expression with operators.
opExpr :: Parser Ast
opExpr = makeExprParser baseExpr undefined