module Sigma.Rule.Internal
  ( Condition(..)
  , Quantity(..)
  , SearchIdPattern(..)
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
  | Of Quantity SearchIdPattern
  | Search SearchIdPattern

-- | Quantity in @1 of xyz@ and @all of xyz@ patterns. 
data Quantity = One | All

-- | A pattern for matching search IDs. Technically, Sigma allows
-- wildcards to show up anywhere in the pattern, but in practice,
-- rule authors only put them at the end (e.g. @selection_*@).
-- To simplify the design and use of this library, only the common
-- case is supported.
data SearchIdPattern
  = SearchIdExact !Text
  | SearchIdPrefix !Text
