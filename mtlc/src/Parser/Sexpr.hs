module Parser.Sexpr
    ( Symbol(..)
    , Sexpr(..)
    , Atom(..)
    ) where

import Data.Text (Text)
import Data.Word (Word64)

newtype Symbol = Symbol Text
  deriving (Show, Eq)

data Sexpr =
    SList [Sexpr]
  | SAtom Atom
  | SLambda [Symbol] Sexpr
  | SIf Sexpr Sexpr Sexpr
  | SDefine Symbol Sexpr
  deriving (Show, Eq)

data Atom =
    AInteger Word64
  | AString  Text
  | ASymbol  Symbol
  | ADouble  Double
  deriving (Show, Eq)
