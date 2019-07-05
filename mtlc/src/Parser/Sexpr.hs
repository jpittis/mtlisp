-- | AST datatypes for s-expressions.
module Parser.Sexpr
    ( Symbol(..)
    , Sexpr(..)
    , Atom(..)
    ) where

import Data.Text (Text)
import Data.Word (Word64)

newtype Symbol = Symbol Text
  deriving (Show, Eq, Read)

data Sexpr =
    SList [Sexpr]
  | SAtom Atom
  | SLambda [Symbol] Sexpr
  | SIf Sexpr Sexpr Sexpr
  | SDefine Symbol Sexpr
  deriving (Show, Eq, Read)

data Atom =
    AInteger Word64
  | AString  Text
  | ASymbol  Symbol
  | ADouble  Double
  deriving (Show, Eq, Read)
