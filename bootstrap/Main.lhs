====================================================================================================
                                     Ri Language Specification
====================================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        12024 edition draft
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. |em| unicode:: U+2003
.. |en| unicode:: U+2002

This document defines the semantics and behaviour of Rini's programming language. It is merely a
draft, but in its final form it will serve both as a concrete language report, and as a reference
implementation of the language.

> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Use newtype instead of data" #-}
> {-# HLINT ignore "Use <$>" #-}
>
> import Control.Monad (void)
> import Data.Char (isAlpha, isAlphaNum, isLower, isUpper)
> import Numeric (readBin, readHex, readOct)
> import Text.Parsec
>
> type Parser = Parsec String ()
> type Id = String

Lexical Structure
=================

Whitespace
----------

.. todo:: Unicode whitespace?

> whitespace = skipMany (void (oneOf " \t") <|> comment)
>
> comment = do string "/*"; commentBody
>
> commentBody = 
>   do string "*/"; return ()
>   <|> do comment; commentBody
>   <|> do anyChar; commentBody
>   <?> "end of comment"
>
> lexeme :: Parser a -> Parser a
> lexeme p = do
>   x <- p; whitespace
>   return x

Keywords and operators
----------------------

The following keywords are reserved and shall not be used as identifiers:

> reservedIds = words
>   " break     case      continue  defer     do        false     for        \
>   \ func      if        import    let       nil       return    throw      \
>   \ true      unless    until     use       var       while     yield      "

Not all exist in the current version, but may be used in future versions of the language.

> kwCase    = kw "case"
> kwElse    = kw "else"
> kwFalse   = kw "false"
> kwFunc    = kw "func"
> kwIf      = kw "if"
> kwLet     = kw "let"
> kwNil     = kw "nil"
> kwThen    = kw "then"
> kwTrue    = kw "true"
>
> kw :: String -> Parser ()
> kw p = lexeme $ do string p; notFollowedBy idChar

Operator precedence is defined in `Section 1.5 <#expressions>`__

> plus    = op "+"
> minus   = op "-"
> star    = op "*"
> slash   = op "/"
>
> colon   = op ":"
>
> op :: String -> Parser String
> op p = lexeme $ string p

Any 2-adic function may be used infix like an operator, by surrounding it in backticks (`` ` ``):

> quotedOp = lexeme $ do
>   quote; i <- identifier; quote
>   return i
>
> quote = char '`'

Identifiers
-----------

Identifiers start with a letter followed by any number of letters, digits and underscores, according
to their `Unicode Classes <http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table>`_:

> identifier :: Parser Id
> identifier = lexeme $ do
>   x   <- idStart
>   xs  <- many idChar
>   if x : xs `elem` reservedIds
>     then unexpected "keyword"
>     else return (x : xs)
>
> idStart = satisfy isAlpha
> idChar  = satisfy isAlphaNum <|> char '_'

A type's identifier must start with an uppercase letter, while other identifiers must start with a
lowercase letter:

> typeIdentifier = do
>   i <- identifier
>   if isUpper (head i)
>     then return i
>     else fail "types must start with an uppercase letter"
>
> varIdentifier = do
>   i <- identifier
>   if isLower (head i)
>     then return i
>     else fail "variables must start with a lowercase letter"

Literals
--------

> data Const
>   = IntConst Integer

Numeric literals
~~~~~~~~~~~~~~~~

> constant :: Parser Const
> constant = choice
>   [ IntConst <$> integer
>   ]
>
> integer = do
>   d   <- digit
>   ds  <- many1 digit
>   return (read (d : ds))

Expressions
-----------

.. topic:: Grammar

    =============== ===== ======================= ===
    *expression*    `=`   *binexp* ``:`` *type*   (type annotation)
    ..              `|`   *binexp*                ..
    ..              ..    ..                      ..
    *binexp*        `=`   *binexp* ``+`` *mulexp* ..
    ..              `|`   *binexp* ``-`` *mulexp* ..
    ..              `|`   *mulexp*                ..
    ..              ..    ..                      ..
    *mulexp*        `=`   *mulexp* ``*`` *quoexp* ..
    ..              `|`   *mulexp* ``/`` *quoexp* ..
    ..              `|`   *quoexp*                ..
    ..              ..    ..                      ..
    *quoexp*        `=`   *priexp* *qop* *priexp* (quoted function call)
    ..              ..    ..                      ..
    *qop*           `=`   `` ` `` *id* `` ` ``    ..
    =============== ===== ======================= ===

> expression :: Parser Expr
> expression = 
>   do 
>     e <- addExpr; colon; t <- typeExpr
>     return (SigExpr e t)
>   <|> addExpr
>
> addExpr   = mulExpr   `chainl1` operators [plus, minus]
> mulExpr   = quotExpr  `chainl1` operators [star, slash]
> quotExpr  = primExpr  `chainl1` operators [quotedOp]
>
> operators :: [Parser Id] -> Parser (Expr -> Expr -> Expr)
> operators p = choice $ map (fmap BinExpr) p
>
> preExpr =
>   do 
>     p <- preOp; e <- preExpr
>     return (PreExpr p e)
>   <|> primExpr
>
> preOp = choice [plus, minus]
>
> primExpr = ConstExpr <$> constant

.. topic:: Translation

    ======================= === ===
    | ``if`` *e₁* ``then``  `=` | ``case`` *e₁* ``{``
    | |em|*e₂*                  | |em|``true``|en| ``->`` *e₂*``;``
    | ``else``                  | |em|``false`` ``->`` *e₃*``;``
    | |em|*e₃*                  | ``}``
    ======================= === ===

> ifElseExpr = do
>   kwIf;   e <- expression
>   kwThen; t <- expression
>   kwElse; f <- expression
>   return (CaseExpr e [undefined, undefined])

> data Expr
>   = ConstExpr Const           -- literal
>   | VarExpr   Id              -- variable
>   | ApplyExpr Expr [Expr]     -- function application  (f(e1, ..., eN))
>   | CaseExpr  Expr [Alt]      -- case expression (case e { alt1, ..., altN })
>   | LamExpr   [Pat] Expr      -- anonymous function (\p1, ..., pN -> e)
>   | PreExpr   Id Expr         -- prefix operator (+e)
>   | BinExpr   Id Expr Expr    -- infix operator (e1 + e2)
>   | SigExpr   Expr Type       -- type annotation (e : t)
>
> data Alt

Patterns
--------

> data Pat

Types
-----

> typeExpr = undefined
> data Type
