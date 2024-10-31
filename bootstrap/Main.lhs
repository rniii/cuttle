====================================================================================================
                                     Ri Language Specification
====================================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        12024 edition draft
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This document defines the semantics and behaviour of Rini's programming language. It is merely a
draft, but in its final form it will serve both as a concrete language report, and as a reference
implementation of the language.

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

.. todo::
  
   Unicode whitespace?

> whitespace = skipMany1 (oneOf " \t")

> lexeme :: Parser a -> Parser a
> lexeme p = do
>   x <- p; whitespace; return x
>
> punctuation :: String -> Parser ()
> punctuation p = string p >> whitespace

Keywords
--------

The following keywords are reserved and shall not be used as identifiers:

> reservedIds = words
>   " break     case      continue  defer     do        false     for        \
>   \ fn        if        import    let       nil       return    throw      \
>   \ true      unless    until     use       var       while     yield      "

Identifiers
-----------

Identifiers start with a letter followed by any number of letters, digits and underscores, according
to their Unicode classes[#ucd]_:

.. [#ucd] http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table

> identifier :: Parser Id
> identifier = lexeme $ do
>   x   <- satisfy isAlpha
>   xs  <- many (satisfy isAlphaNum <|> char '_')
>   if x : xs `elem` reservedIds
>     then unexpected "keyword"
>     else return (x : xs)

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

> expression :: Parser Expr
> expression = sigExpr <|> binExpr

> sigExpr = do
>   e <- binExpr; punctuation ":"; t <- undefined
>   return (SigExpr e t)

> binExpr = primExpr

> primExpr = ConstExpr <$> constant

> data Expr
>   = ConstExpr Const           -- literal
>   | VarExpr   Id              -- variable
>   | ApplyExpr Expr [Expr]     -- function application  (f(e1, ..., eN))
>   | CaseExpr  Expr [Alt]      -- case expression (case e { alt1, ..., altN })
>   | IfExpr    Expr Expr Expr  -- conditional (if e1 then e2 else e3)
>   | LamExpr   [Pat] Expr      -- anonymous function (\p1, ..., pN -> e)
>   | PreExpr   Id Expr         -- prefix operator (+e)
>   | BinExpr   Id Expr Expr    -- infix operator (e1 + e2)
>   | SigExpr   Expr Type       -- type annotation (e : t)
>
> data Alt
>
> data Pat
>
> data Type
