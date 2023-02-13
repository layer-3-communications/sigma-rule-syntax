module Sigma.Rule.Internal
  ( Condition(..)
  , Connective(..)
  , SearchPattern(..)
  , Token(..)
  ) where

import Data.Text (Text)

-- Used internally for tokenization of condition.
data Token
  = TokenAnd
  | TokenOr
  | TokenNot
  | TokenOf
  | TokenOne
  | TokenAll
  | TokenOpenParen
  | TokenCloseParen
  | TokenEof
  | TokenId !Text
  | TokenIdPrefix !Text
  deriving (Show)

-- | A Sigma condition. This library does not support using the pipe
-- operator for aggregations. 
data Condition
  = And Condition Condition -- Prec 4
  | Or Condition Condition -- Prec 3
  | Not Condition -- Prec 5
  | Of Connective SearchPattern
  | Reference SearchPattern

-- | The two logical connectives used in Sigma rules.
-- When this appears in @x of y@ patterns, the values of @x@ should be
-- interpreted as:
--
-- * 1: disjunction
-- * all: conjunction
data Connective = Conjunction | Disjunction

-- | A pattern for matching search IDs. Technically, Sigma allows
-- wildcards to show up anywhere in the pattern, but in practice,
-- rule authors only put them at the end (e.g. @selection_*@).
-- To simplify the design and use of this library, only the common
-- case is supported.
data SearchPattern
  = SearchExact !Text
  | SearchPrefix !Text
