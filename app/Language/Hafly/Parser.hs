
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
import Control.Applicative (Const (getConst))
import Data.Dynamic
import qualified Language.Hafly.Ast as Ast

type Parser = Parsec Void T.Text

parseExpression :: [[Operator (Const (String, Dynamic)) Void]] -> T.Text -> Either (ParseErrorBundle T.Text Void) Ast
parseExpression opDefs = parse (opExpr (getOperatorDefs opDefs) <* eof) ""

parseProgram :: [[Operator (Const (String, Dynamic)) Void]] -> T.Text -> Either (ParseErrorBundle T.Text Void) Program
parseProgram opDefs = parse (program $ getOperatorDefs opDefs) ""

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

getOperatorDefs :: [[Operator (Const (String, Dynamic)) Void]] -> [[Operator Parser Ast]]
getOperatorDefs = fmap (fmap toParser)
  where
    toParser :: Operator (Const (String, Dynamic)) Void -> Operator Parser Ast
    toParser = \case
        InfixN co  -> InfixN (parserFor $ getConst co)
        InfixL co  -> InfixL (parserFor $ getConst co)
        InfixR co  -> InfixR (parserFor $ getConst co)
        Prefix co  -> Prefix (unaryParserFor $ getConst co)
        Postfix co -> Postfix (unaryParserFor $ getConst co)
        TernR co   -> error "Unsupported operator type"

    parserFor :: (String, Dynamic) -> Parser (Ast -> Ast -> Ast)
    parserFor (op, res) = do
        token $ string (T.pack op)
        pure $ App . App (Ast.Const res)

    unaryParserFor :: (String, Dynamic) -> Parser (Ast -> Ast)
    unaryParserFor (op, res) = undefined

-- Parser for an entire hafly program.
program :: [[Operator Parser Ast]] -> Parser Program
program opDefs = Program . fromList <$>
    sepBy (exprDef opDefs) (token newline)

exprDef :: [[Operator Parser Ast]] -> Parser (String, Ast)
exprDef opDefs = do
    x <- identifier
    equals
    y <- expr opDefs
    return (x, y)

-- | Parse an arbitrary hafly expression.
expr :: [[Operator Parser Ast]] -> Parser Ast
expr opDefs = try (app opDefs)
  <|> baseExpr opDefs

baseExpr :: [[Operator Parser Ast]] -> Parser Ast
baseExpr opDefs = try (Literal <$> literal)
    <|> try (lambdaExpr opDefs)
    <|> try (Var <$> identifier)
    <|> (Sequence <$> block opDefs)

lambdaExpr :: [[Operator Parser Ast]] -> Parser Ast
lambdaExpr opDefs = do
    lambdaToken
    vars <- sepBy identifier whitespace
    arrToken
    body <- opExpr opDefs
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
block :: [[Operator Parser Ast]] -> Parser SequenceAst
block opDefs = do
    leftBracket
    res <- sepBy1 (sequenceExpr opDefs) (token semicolon)
    rightBracket
    return $ SequenceAst res

sequenceExpr :: [[Operator Parser Ast]] -> Parser SequenceExpr
sequenceExpr opDefs = try (bindExpr opDefs)
    <|> Expr <$> expr opDefs

bindExpr :: [[Operator Parser Ast]] -> Parser SequenceExpr
bindExpr opDefs = do
    x <- identifier
    bindToken
    y <- expr opDefs
    return $ BindExpr x y

-- Parse a function application.
app :: [[Operator Parser Ast]] -> Parser Ast
app opDefs = foldl1 App <$>
    some (try (baseExpr opDefs) <|> inParens (opExpr opDefs))

-- Parse an expression with operators.
opExpr :: [[Operator Parser Ast]] -> Parser Ast
opExpr opDefs = makeExprParser (expr opDefs) opDefs