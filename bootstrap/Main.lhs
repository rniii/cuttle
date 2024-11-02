====================================================================================================
                                     Ri Language Specification
====================================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        12024 edition draft
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:Author: S\. Rini et soror

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
>
> main :: IO ()
> main = do
>   i <- getContents
>   case parse expression "" i of
>     Right x -> print x
>     Left  e -> print e
>
> instance Show Const where
>   show (IntConst i) = show i
> instance Show Expr where
>   show (ConstExpr e)    = show e
>   show (VarExpr e)      = e
>   show (ApplyExpr f a)  = show f ++ "(" ++ show a ++ ")"
>   show (BinExpr o a b)  = "(" ++ show a ++ " " ++ o ++ " " ++ show b ++ ")"
>   show (SigExpr e t)    = show e ++ " : " ++ show t
>   show _ = "?"
>
> deriving instance (Show Alt)
> deriving instance (Show Type)
> deriving instance (Show Pat)

Introduction
============

Program Structure
-----------------

.. todo:: modules

Source files must be UTF-8 encoded, unless it is not reasonable for the environment. The syntax
depends on categories assigned by the Unicode consortium. Implementations are expected to use newer
Unicode versions as they become available.

Notation
--------

The grammar is defined throughout the specification using notation similar to ABNF:

================= ===
*e* = *pattern*   production rule
*pat*₁ *pat*₂     sequence
[*pattern*]       option
{*pattern*}       zero or more repetitions
(*pattern*)       grouping
*pat*₁ | *pat*₂   alternation
``kw``            terminal token (monospace font)
*exp*             non-terminal token (italics)
================= ===

Lexical Structure
=================

This chapter describes the language's grammar.

In the provided implementation, no tokenization is done. This is simply to make handling of
whitespace more concrete, and an implementation may use any sort of parser.

Whitespace
----------

.. topic:: Grammar

    =============== === ===
    *ws*            `=` {*space*}
    ..              ..  ..
    *lws*           `=` {*space* | *newline* | *comment*}
    ..              ..  ..
    *comment*       `=` ``--`` any:sub:`(except LF or CRLF)` *newline*
    ..              ..  ..
    *space*         `=` #x20 | #x09
    ..              ..  ..
    *newline*       `=` #x0a | (#x0d #x0a)
    ..              ..  ..
    *eos*           `=` *newline* *lws* | ``;`` *lws*
    =============== === ===

.. TODO:: Unicode whitespace is probably a bad idea; what about ascii VT/FF?

Instead of doing automatic semicolon insertion, there are two kinds of whitespace: regular
whitespace and linear whitespace.

Each lexeme may be followed by regular whitespace, but linear whitespace is only used in some parts
of the grammar (e.g. ``x = a\n  + b``). Statements are terminated either by newlines or by
semicolons.

Regular whitespace
  Any amount of ASCII spaces ``' '`` and tabs ``'\t'``

Linear whitespace
  Any amount of regular whitespace, newlines, and line comments ``-- …``

Newline
  ASCII LF ``'\n'``, CRLF ``"\r\n"``, or CR ``'\r'`` (discouraged)

Statement terminator
  Either a newline, or a semicolon ``;``

Comments can be nested. ``/* /* … */ */`` is a single comment.

> ws  = skipMany $ void spc
> lws = skipMany $ void spc <|> void eol <|> comment
>
> comment = void $ do string "--"; many (noneOf "\r\n"); eol
>
> spc = oneOf " \t"
> eol = crlf <|> oneOf "\n\r"
>
> lexeme :: Parser a -> Parser a
> lexeme p = do
>   x <- p; ws
>   return x

Keywords and Operators
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

Operator precedence is defined in `Section 3 <#expressions>`__

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
>
> literal :: Parser Const
> literal = choice
>   [ IntConst <$> integer
>   ]

Numeric literals
~~~~~~~~~~~~~~~~

> integer = lexeme $ do
>   d   <- digit
>   ds  <- many digit
>   return (read (d : ds))

Expressions
===========

.. topic:: Grammar

    =================== ===== ========================= ===
    *expression*        `=`   *aexp* ``:`` *type*       (type annotation)
    ..                  `|`   *aexp*                    ..
    ..                  ..    ..                        ..
    *aexp*              `=`   *aexp* ``+`` *mexp*       ..
    ..                  `|`   *aexp* ``-`` *mexp*       ..
    ..                  `|`   *mexp*                    ..
    ..                  ..    ..                        ..
    *mexp*              `=`   *mexp* ``*`` *qexp*       ..
    ..                  `|`   *mexp* ``/`` *qexp*       ..
    ..                  `|`   *mexp* ``//``             ..
    ..                  `|`   *qexp*                    ..
    ..                  ..    ..                        ..
    *qexp*              `=`   *primexp* *qop* *quotexp* (quoted function call)
    ..                  `|`   *primexp*                 ..
    ..                  ..    ..                        ..
    *qop*               `=`   `` ` `` *id* `` ` ``      ..
    =================== ===== ========================= ===

Operator expressions
~~~~~~~~~~~~~~~~~~~~

> expression :: Parser Expr
> expression = 
>   try (do 
>     e <- addExpr; colon; t <- typeExpr
>     return (SigExpr e t))
>   <|> addExpr
>
> addExpr   = mulExpr   `chainl1` operators [plus, minus]
> mulExpr   = quotExpr  `chainl1` operators [star, slash]
> quotExpr  = preExpr   `chainl1` operators [quotedOp]
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
> primExpr = ConstExpr <$> literal

.. topic:: Translation

    ======================= === ===
    | ``if`` *e*₁ ``then``  `=` | ``case`` *e*₁ ``{``
    | |em|*e*₂                  | |em|``true``|en| ``->`` *e*₂``;``
    | ``else``                  | |em|``false`` ``->`` *e*₃``;``
    | |em|*e*₃                  | ``}``
    ======================= === ===

> ifElseExpr = do
>   kwIf;   e <- expression
>   kwThen; t <- expression
>   kwElse; f <- expression
>   return (CaseExpr e [undefined, undefined])

> data Expr
>   = ConstExpr Const           -- literal
>   | VarExpr   Id              -- variable
>   | ApplyExpr Expr [Expr]     -- function application  (f(e₁, …, eₙ))
>   | CaseExpr  Expr [Alt]      -- case expression (case e { alt₁, …, altₙ })
>   | LamExpr   [Pat] Expr      -- anonymous function (\p₁, …, pₙ -> e)
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

> typeExpr = ConstrType <$> identifier
>
> data Type =
>   ConstrType Id

Declarations
============

> data Decl

Standard types
==============

The optional type: ``T?``
~~~~~~~~~~~~~~~~~~~~~~~~~


``Equatable``
~~~~~~~~~~~~~

::

  class Equatable {
    func (==)(a: Self, b: Self) -> Bool {
      not a != b
    }

    func (!=)(a: Self, b: Self) -> Bool {
      not a == b
    }
  }

  default instance for Equatable { @builtin() } // derived by compiler

``Ordered``
~~~~~~~~~~~

::

  data Ordering: Ordered, Equatable, Show {
    Less = -1
    Equal
    Greater
  }

  class Ordered: Equatable {
    func compare(a: Self, b: Self) -> Ordering {
      if a == b
        then Equal
      elif a <= b
        then Less
        else Greater
    }

    func (<=)(a, b) {
      compare(a, b) is not .greater
    }

    func (>=)(a, b) {
      b <= a
    }

    func (<)(a, b) {
      not b <= a
    }

    func (>)(a, b) {
      not a <= b
    }
  }

  default instance for Equatable { @builtin() } // derived by compiler
