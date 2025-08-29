{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}

module Sigma.Rule.Internal
  ( Condition(..)
  , Connective(..)
  , SearchPattern(..)
  , Token(..)
  , Resolution(..)
  , Search(..)
  , Match(..)
  , Literal(..)
  , Modifier(..)
  , parseSearch
  ) where

import Data.Text (Text)
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Set (Set)
import Data.Int (Int64)
import Data.Aeson (FromJSON,ToJSON)
import Data.Foldable (foldl')
import Data.Foldable (foldlM)

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Set as Set
import qualified Data.Scientific as SCI
import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Map.Strict as M

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

data Resolution = Resolved | Unresolved

-- | A Sigma condition. This library does not support using the pipe
-- operator for aggregations. 
data Condition :: Resolution -> Type where
  And :: -- Prec 4
       !(Condition r)
    -> !(Condition r)
    -> Condition r
  Or :: -- Prec 3
       !(Condition r)
    -> !(Condition r)
    -> Condition r
  Not :: -- Prec 5
       !(Condition r)
    -> Condition r
  Of :: -- ^ Corresponds to @all of@, @1 of@.
       !Connective
    -> !SearchPattern
    -> Condition 'Unresolved
  Reference ::
       !Text -- exact match only
    -> Condition 'Unresolved
  ResolvedOf :: 
       !Connective
    -> !(Vector Search)
    -> Condition 'Resolved
  ResolvedReference :: 
       !Search
    -> Condition 'Resolved

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

-- | In a yaml file, this can look like this
--
-- > domain_name:
-- >   - 'badguy.example.com'
-- >   - 'malicious.example.com'
--
-- or like this
--
-- > domain_name: 'dangerous.example.com'
data Match = Match
  { field :: !Text
  , modifiers :: !(Set Modifier)
    -- ^ Modifiers. These are introduces by vertical bars in the yaml
    -- files. For example: @domain_name|endswith@. 
  , values :: !(Vector Literal)
    -- ^ Values that the field is expected to match.
  }

data Search = Search
  { connective :: !Connective
    -- ^ What the Sigma spec calls @{map-list}@ means disjunction, and
    -- what it calls @{field: value}@ means conjunction.
  , matches :: !(Vector Match)
  }

-- | The value used for a match. The Sigma specification is not
-- clear about what kinds of values are allowed, so this may
-- be wrong.
data Literal
  = String !Text
  | Number !Int64
  | Null
  deriving stock (Eq)

data Modifier
  = EndsWith
  | Contains
  | StartsWith
  | Base64Offset
  | AllModifier
    -- ^ The @all@ modifier. This is suffixed with the word modifier
    -- to prevent a naming conflict with the @All@ data constructor
    -- of @Quantity@.
  deriving (Show,Eq,Ord)

instance ToJSON Literal where
  toJSON = \case
    String s -> Aeson.String s
    Number n -> Aeson.toJSON n
    Null -> Aeson.Null

instance ToJSON Search where
  toJSON s = case s.connective of
    Conjunction -> Aeson.toJSON $ foldl'
      (\acc m -> M.insert (m.field <> modsToText m.modifiers) (Aeson.toJSON m.values) acc
      ) M.empty s.matches
    Disjunction -> Aeson.toJSON $ fmap 
      (\m -> M.singleton (m.field <> modsToText m.modifiers) m.values)
      s.matches

modsToText :: Set Modifier -> Text
modsToText = foldr
  (\m acc ->
    let enc = case m of
          EndsWith -> "|endswith"
          StartsWith -> "|startswith"
          Base64Offset -> "|base64offset"
          Contains -> "|contains"
          AllModifier -> "|all"
     in enc <> acc
  ) T.empty

instance FromJSON Search where
  parseJSON = parseSearch

parseSearch :: Aeson.Value -> Aeson.Parser Search
parseSearch = \case
  Aeson.Array v -> Search Disjunction <$> parseMatchesArray v
  Aeson.Object m -> Search Conjunction <$> parseMatchesObject m
  _ -> fail "expected search to be array or object"

parseMatchesObject :: KeyMap.KeyMap Aeson.Value -> Aeson.Parser (Vector Match)
parseMatchesObject m = do
  result <- foldlM
    (\acc (k,v) -> do
      (field,modifiers) <- parseMatchField k
      values <- generousLiteralArrayParser v
      pure (Match{field,modifiers,values} : acc)
    ) [] xs
  pure (V.fromList (List.reverse result))
  where
  xs = KeyMap.toAscList m

parseMatchesArray :: Vector Aeson.Value -> Aeson.Parser (Vector Match)
parseMatchesArray = traverse parseSingletonMatchObject

parseSingletonMatchObject :: Aeson.Value -> Aeson.Parser Match
parseSingletonMatchObject = Aeson.withObject "Match" $ \m -> case KeyMap.toList m of
  [(k,v)] -> do
    (field,modifiers) <- parseMatchField k
    values <- generousLiteralArrayParser v
    pure Match{field,modifiers,values}
  _ -> fail "expected match object to have a single member" 

generousLiteralArrayParser :: Aeson.Value -> Aeson.Parser (Vector Literal)
generousLiteralArrayParser = \case
  Aeson.Array xs -> traverse generousLiteralParser xs
  v -> fmap V.singleton (generousLiteralParser v)

generousLiteralParser :: Aeson.Value -> Aeson.Parser Literal
generousLiteralParser = \case
  Aeson.String t -> pure (String t)
  Aeson.Number sci -> case SCI.toBoundedInteger sci of
    Nothing -> fail "literal integer out of 64-bit signed range"
    Just i -> pure (Number i)
  Aeson.Null -> pure Null
  _ -> fail "literal: expected null, string, or number"

parseMatchField :: Key.Key -> Aeson.Parser (Text, Set Modifier)
parseMatchField k = case T.splitOn "|" t of
  [] -> fail "empty field name not allowed"
  x : xs -> do
    mods <- foldlM
      (\acc modText -> case decodeModText modText of
        Nothing -> fail "unrecognized modifier"
        Just modifier -> pure (Set.insert modifier acc)
      ) Set.empty xs
    pure (x,mods)
  where
  t = Key.toText k

decodeModText :: Text -> Maybe Modifier
decodeModText = \case
  "endswith" -> Just EndsWith
  "startswith" -> Just StartsWith
  "base64offset" -> Just Base64Offset
  "contains" -> Just Contains
  "all" -> Just AllModifier
  _ -> Nothing

