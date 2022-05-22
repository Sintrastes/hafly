
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

bindToken = token (string "<-")

semicolon = token (char ';')

identifier = token (some alphaNumChar)

-- Parser for an entire hafly program.
program :: Parser Program
program = undefined

-- | Parse an arbitrary hafly expression.
expr :: Parser Ast
expr = try app
  <|> baseExpr

baseExpr :: Parser Ast
baseExpr = try (Literal <$> literal)
    <|> try (Atom <$> identifier)
    <|> (Sequence <$> block)

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
    some baseExpr

-- Parse an expression with operators.
opExpr :: Parser Ast
opExpr = makeExprParser baseExpr undefined