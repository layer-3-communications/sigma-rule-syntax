{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Sigma.Rule.Tokenize
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  ) where

import Sigma.Rule.Internal
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
}
%wrapper "monad-bytestring"
%encoding "utf-8"

$digit = [0-9]
$alpha = [a-zA-Z]
@id = ($alpha | \_) ($alpha | $digit | \* | \_ | \' | \?)*

tokens :-

<0> $white+ ;
<0> "and"   { \_ _ -> pure TokenAnd }
<0> "or"    { \_ _ -> pure TokenOr }
<0> "not"   { \_ _ -> pure TokenNot }
<0> "1"     { \_ _ -> pure TokenOne }
<0> "of"    { \_ _ -> pure TokenOf }
<0> "("     { \_ _ -> pure TokenOpenParen }
<0> ")"     { \_ _ -> pure TokenCloseParen }
<0> "all"   { \_ _ -> pure TokenAll }
<0> @id     { \(_,_,b,_) len ->
              case TE.decodeUtf8' (LBS.toStrict (LBS.take len b)) of
                Left{} -> alexFail "encountered non-utf-8 string"
                Right t -> case T.unsnoc t of
                  Just (x,'*') -> if T.any (=='*') x
                    then alexFail "non-terminal wildcard not supported"
                    else pure (TokenIdPrefix x)
                  _ -> if T.any (=='*') t
                    then alexFail ("non-terminal wildcard not supported, saw: " ++ T.unpack t)
                    else pure (TokenId t)
            }
{

alexFail :: String -> Alex a
alexFail e = Alex (\_ -> Left e)

alexEOF :: Alex Token
alexEOF = pure TokenEof
}
