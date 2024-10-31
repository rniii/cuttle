module Main (main) where

import Control.Applicative (Applicative (liftA2), liftA3)
import Text.Parsec

main :: IO ()
main =
  case parseModule src of
    Right mod -> print mod
    Left err -> do
      print err
  where
    src = "{ match foo {}; 2 + 2 }"

parseModule :: String -> Either ParseError Expr
parseModule = parse mod "unknown.ct"
  where
    mod = expr

    expr = binExpr

    binExpr = foldl chainl1 prefixExpr binOp
    binOp =
      fmap BinExpr
        <$> [ lexeme (identifier `quotedBy` char '`')
            , choice [punct "*", punct "/"]
            , choice [punct "+", punct "-"]
            ]

    prefixExpr = liftA2 PrefixExpr prefixOp primExpr <|> primExpr <?> "expression"
    prefixOp = choice [punct "+", punct "-"]

    primExpr =
      choice
        [ try caseExpr
        , try ifExpr
        , BlockExpr <$> block
        , ConstExpr <$> const'
        , VarExpr <$> identifier
        ]

    caseExpr = liftA2 CaseExpr (case' >> expr) caseBlock
    caseArm = liftA3 (,,) undefined (optionMaybe guard) expr
    caseBlock = braces (caseArm `sepBy` eol)
    guard = if' >> expr

    ifExpr = liftA3 IfExpr (if' >> expr) block (optionMaybe elseExpr)
    elseExpr = else' >> ((: []) <$> ifExpr <|> block)

    block = braces (expr `sepBy` eol)
    braces = between (punct "{") (punct "}")

    const' = IntConst <$> intConst
    intConst = lexeme (read <$> many1 digit)

    identifier = invalidId <|> lexeme (liftA2 (:) idStart (many idChar)) <?> "identifier"
    idStart = letter :: Parser Char
    idChar = alphaNum <|> char '_' :: Parser Char
    invalidId = anyKeyword >> unexpected "keyword"

    case' = keyword "case"
    if' = keyword "if"
    else' = keyword "else"
    anyKeyword = case' <|> if' <|> else'
    keyword t = string t <* eot <* skip
    punct t = string t <* skip
    quotedBy p q = between q q p

    eot = notFollowedBy idChar
    eol = lexeme (endOfLine <|> char ';')
    lexeme = (<* skip)
    skip = skipMany (char ' ') <?> ""

type Parser = Parsec String ()

data Expr
  = CaseExpr Expr [(Pattern, Maybe Expr, Expr)]
  | IfExpr Expr [Expr] (Maybe [Expr])
  | BlockExpr [Expr]
  | VarExpr Identifier
  | ConstExpr Const
  | BinExpr Op Expr Expr
  | PrefixExpr Op Expr
  deriving (Show)

type Op = String

data Pattern
  = VarPat Identifier
  | ConstPat Const
  | AnyPat
  deriving (Show)

data Const
  = IntConst Integer
  | CharConst Char
  deriving (Show)

type Identifier = String
