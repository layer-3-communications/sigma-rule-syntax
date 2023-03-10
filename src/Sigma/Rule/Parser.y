{
{-# LANGUAGE DeriveFoldable #-}
module Sigma.Rule.Parser
  ( parseSigma
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

import Sigma.Rule.Internal (Token(..))
import qualified Sigma.Rule.Internal as C

}

%name parseSigma Complete
%tokentype { Token }
%error { parseError }
%monad { Either String } { >>= } { pure }
%expect 0

%token
  and   { TokenAnd }
  or    { TokenOr }
  not   { TokenNot }
  one   { TokenOne }
  of    { TokenOf }
  open  { TokenOpenParen }
  close { TokenCloseParen }
  all   { TokenAll }
  ident { TokenId $$ }
  prefix { TokenIdPrefix $$ }
  eof    { TokenEof }

%%

Complete
  : Cond0 eof { $1 }

Cond0
  : open Cond0 close { $2 }
  | Cond0 or Cond1   { C.Or $1 $3 }
  | Cond1 and Cond2  { C.And $1 $3 }
  | not Cond3  { C.Not $2 }
  | QuantityOf { $1 }

Cond1
  : open Cond0 close { $2 }
  | Cond1 and Cond2  { C.And $1 $3 }
  | not Cond3  { C.Not $2 }
  | QuantityOf { $1 }

Cond2
  : open Cond0 close { $2 }
  | not Cond3  { C.Not $2 }
  | QuantityOf { $1 }

Cond3
  : open Cond0 close { $2 }
  | QuantityOf { $1 }

QuantityOf
  : one of ident { C.Of C.Disjunction (C.SearchExact $3) }
  | all of ident { C.Of C.Conjunction (C.SearchExact $3) }
  | one of prefix { C.Of C.Disjunction (C.SearchPrefix $3) }
  | all of prefix { C.Of C.Conjunction (C.SearchPrefix $3) }
  | ident { C.Reference (C.SearchExact $1) }
  | prefix { C.Reference (C.SearchPrefix $1) }

{
parseError :: [Token] -> Either String a
parseError _ = Left "could not parse rule"
}
