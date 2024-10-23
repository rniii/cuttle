{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2), liftA3)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec

main :: IO ()
main = do
  print (parseModule "if true { 1 + 2 } else { 3 }")

parseModule :: String -> Either ParseError Expr
parseModule = parse mod "unknown.ct"
  where
    mod = expr

    expr = binExpr

    binExpr = foldl chainl1 prefixExpr binOps
    binOps =
      [ oprs [(BinExpr MulBin, char '*'), (BinExpr DivBin, char '/')]
      , oprs [(BinExpr AddBin, char '+'), (BinExpr SubBin, char '-')]
      ]

    prefixExpr = liftA2 PrefixExpr prefixOp primExpr <|> primExpr
    prefixOp = oprs [(PosPrefix, char '+'), (NegPrefix, char '-')]

    oprs = (<* skip) . choice . map (uncurry (flip (>>) . return))

    primExpr =
      choice
        [ caseExpr
        , ifExpr
        , ConstExpr <$> const'
        , VarExpr <$> identifier
        ]

    caseExpr = liftA2 CaseExpr (case' >> expr) caseBlock
    caseArm = liftA3 (,,) undefined (optionMaybe guard) expr
    caseBlock = braces (caseArm `sepBy` eol)
    guard = if' >> expr

    ifExpr = liftA3 IfExpr (if' >> expr) block (optionMaybe ifElse)
    ifElse = else' >> block

    block = braces (expr `sepBy` eol)
    braces = between lbrace rbrace

    const' = IntConst <$> intConst
    intConst = read <$> many1 digit <* skip

    identifier = Identifier . T.pack <$> liftA2 (:) idStart (many idChar) <* skip <?> "identifier"
    idStart = letter
    idChar = alphaNum <|> char '_'

    lbrace = char '{' >> skip
    rbrace = char '}' >> skip
    case' = token "case"
    if' = token "if"
    else' = token "else"
    token t = string t >> eot >> skip

    skip = skipMany (char ' ')
    eot = notFollowedBy idChar
    eol = endOfLine

data Expr
  = CaseExpr Expr [(Pattern, Maybe Expr, Expr)]
  | IfExpr Expr [Expr] (Maybe [Expr])
  | VarExpr Identifier
  | ConstExpr Const
  | BinExpr BinOp Expr Expr
  | PrefixExpr PrefixOp Expr

instance Show Expr where
  show (IfExpr e b x) = concat ["(if ", show e, " ", show b, maybe "" (mappend " " . show) x, ")"]
  show (VarExpr v) = show v
  show (ConstExpr c) = show c
  show (BinExpr op a b) = concat ["(", show op, " ", show a, " ", show b, ")"]
  show (PrefixExpr op a) = concat ["(", show op, " ", show a, ")"]
  show _ = "?"

data BinOp
  = AddBin
  | SubBin
  | MulBin
  | DivBin

instance Show BinOp where
  show AddBin = "+"
  show SubBin = "-"
  show MulBin = "*"
  show DivBin = "/"

data PrefixOp
  = NegPrefix
  | PosPrefix
  deriving (Show)

data Pattern
  = VarPat Identifier
  | ConstPat Const
  | AnyPat
  deriving (Show)

data Const
  = IntConst Integer
  | CharConst Char

instance Show Const where
  show (IntConst c) = show c
  show (CharConst c) = show c

newtype Identifier = Identifier Text

instance Show Identifier where
  show (Identifier i) = T.unpack i
