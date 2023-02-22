{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}

module Sigma.Rule
  ( Rule(..)
  , Status(..)
  , Logsource(..)
  , Modifier(..)
  , Literal(..)
  , Match(..)
  , Detection(..)
  , Condition(..)
  , Connective(..)
  , SearchPattern(..)
  , Search(..)
  ) where

import Prelude hiding (id)

import Data.Foldable (foldl')
import Data.Bifunctor (first)
import Data.Aeson (FromJSON,FromJSONKey,ToJSON,(.:),(.:?),(.=))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Sigma.Rule.Internal (Condition(..),SearchPattern(..),Connective(..))

import qualified Data.Scientific as SCI
import qualified Chronos as Chronos
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as LT
import qualified Data.Set as Set
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Sigma.Rule.Internal as Internal
import qualified Sigma.Rule.Parser as Parser
import qualified Data.Text as T
import qualified Sigma.Rule.Tokenize as Tokenize

-- | The status of the rule.
data Status
  = Stable
  | Test
  | Experimental
  | Deprecated
  | Unsupported

data Logsource
  = Category
  | Product
  | Service
  | Definition
  deriving (Eq,Ord)

data Modifier
  = EndsWith
  | Contains
  | StartsWith
  | Base64Offset
  | AllModifier
    -- ^ The @all@ modifier. This is suffixed with the word modifier
    -- to prevent a naming conflict with the @All@ data constructor
    -- of @Quantity@.
  deriving (Eq,Ord)

-- | The value used for a match. The Sigma specification is not
-- clear about what kinds of values are allowed, so this may
-- be wrong.
data Literal
  = String !Text
  | Number !Int64
  | Null
  deriving stock (Eq)

instance FromJSON Status where
  parseJSON = Aeson.withText "Status" $ \t -> case t of
    "stable" -> pure Stable
    "test" -> pure Test
    "experimental" -> pure Experimental
    "deprecated" -> pure Deprecated
    "unsupported" -> pure Unsupported
    _ -> fail "unrecognized status"

instance ToJSON Rule where
  toJSON r = Aeson.object
    [ "title" .= r.title
    , "id" .= r.id
    , "status" .= r.id
    , "author" .= r.author
    , "description" .= r.description
    , "date" .= encodeDate r.date
    , "modified" .= encodeDate r.modified
    , "detection" .= r.detection
    , "logsource" .= r.logsource
    ]

instance FromJSON Rule where
  parseJSON = Aeson.withObject "Rule" $ \m -> do
    title <- m .: "title"
    id <- m .: "id"
    status <- m .: "status"
    description <- m .: "description" 
    author <- m .: "author"
    date <- (m .: "date") >>= parserDate
    modified <- (m .:? "modified") >>= \case
      -- If the rule has never been modified, use the creation
      -- date as the modified date.
      Nothing -> pure date
      Just v -> parserDate v
    logsource <- m .: "logsource"
    detection <- m .: "detection"
    pure Rule{title,id,status,description,author,date,modified,logsource,detection}

instance FromJSONKey Logsource where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \t -> case t of
    "category" -> pure Category
    "product" -> pure Product
    "service" -> pure Service
    "definition" -> pure Definition
    _ -> fail "unrecognized logsource"
  fromJSONKeyList = Aeson.FromJSONKeyTextParser (\_ -> fail "logsource list not supported")

instance ToJSON Logsource where
  toJSON = \case
    Category -> Aeson.String "category"
    Product -> Aeson.String "product"
    Service -> Aeson.String "service"
    Definition -> Aeson.String "definition"

instance Aeson.ToJSONKey Logsource where
  toJSONKey = Aeson.ToJSONKeyText
    (\s -> case s of
      Category -> "category"
      Product -> "product"
      Service -> "service"
      Definition -> "definition"
    )
    (\s -> case s of
      Category -> Encoding.text "category"
      Product -> Encoding.text "product"
      Service -> Encoding.text "service"
      Definition -> Encoding.text "definition"
    )

-- Sigma expects dates to be formatted as YYYY/mm/dd
parserDate :: Text -> Aeson.Parser Chronos.Date
parserDate t =
  case Atto.parseOnly (Chronos.parser_Ymd (Just '/') <* Atto.endOfInput) t of
    Left e -> fail e
    Right d -> pure d

-- Sigma expects dates to be formatted as YYYY/mm/dd
encodeDate :: Chronos.Date -> Text
encodeDate = LT.toStrict . TB.toLazyText . Chronos.builder_Ymd (Just '/')

-- | A rule. Use the @FromJSON@ instance to decode these from the yaml
-- files that sigma provides.
data Rule = Rule
  { title :: !Text
  , id :: !UUID
  , status :: !Status
  , description :: !Text
  , author :: !Text
  , date :: !Chronos.Date
  , modified :: !Chronos.Date
  , logsource :: !(Map Logsource Text)
  , detection :: !Detection
  }

instance FromJSON Detection where
  parseJSON = Aeson.withObject "Detection" $ \m -> do
    condition <- (m .: "condition") >>= parseCondition
    let m1 = KeyMap.toMapText m
    searches <- traverse parseSearch (M.delete "condition" m1)
    pure Detection
      { condition
      , searches
      }

instance FromJSON Search where
  parseJSON = parseSearch

-- | This uses @{map-list}@ to encode everything even when
-- it would be possible to use the @{field: value}@ scheme
-- instead.
instance ToJSON Detection where
  toJSON x = Aeson.object $
    ( "condition" .= encodeCondition 0 x.condition
    ) : M.foldrWithKey (\k v acc -> (Key.fromText k .= v) : acc) [] x.searches

-- instance ToJSON Match where
--   toJSON m = Aeson.object
--     [ Key.fromText (m.field <> modsToText m.modifiers) .= m.values
--     ]

instance ToJSON Search where
  toJSON s = case s.connective of
    Conjunction -> Aeson.toJSON $ foldl'
      (\acc m -> M.insert (m.field <> modsToText m.modifiers) (Aeson.toJSON m.values) acc
      ) M.empty s.matches
    Disjunction -> Aeson.toJSON $ fmap 
      (\m -> M.singleton (m.field <> modsToText m.modifiers) m.values)
      s.matches

instance ToJSON Literal where
  toJSON = \case
    String s -> Aeson.String s
    Number n -> Aeson.toJSON n
    Null -> Aeson.Null

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

encodeCondition :: Int -> Condition -> LT.Text
encodeCondition !prec = \case
  And a b ->
    let x = encodeCondition 4 a <> " and " <> encodeCondition 4 b
     in parens (prec > 4) x
  Or a b ->
    let x = encodeCondition 3 a <> " or " <> encodeCondition 3 b
     in parens (prec > 3) x
  Not a ->
    let x = "not " <> encodeCondition 5 a
     in parens (prec > 5) x
  Reference s -> encodeSearchIdPattern s
  Of qty s ->
    (case qty of {Disjunction -> "1"; Conjunction -> "all"})
    <>
    " of "
    <>
    encodeSearchIdPattern s

parens :: Bool -> LT.Text -> LT.Text
parens needed b = if needed then "(" <> b <> ")" else b

encodeSearchIdPattern :: SearchPattern -> LT.Text
encodeSearchIdPattern = \case
  SearchExact t -> LT.fromStrict t
  SearchPrefix p -> LT.fromStrict p <> "*"

parseCondition :: Aeson.Value -> Aeson.Parser Condition 
parseCondition = Aeson.withText "Condition" $ \t ->
  case decodeCondition t of
    Left e -> fail e
    Right c -> pure c

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

decodeCondition :: Text -> Either String Condition
decodeCondition t = do
  tokens <- Tokenize.runAlex (LBS.fromStrict (TE.encodeUtf8 t)) parseAllTokens
  first
    (\e -> e ++ ", tokenized: " ++ show tokens)
    (Parser.parseSigma tokens)

decodeModText :: Text -> Maybe Modifier
decodeModText = \case
  "endswith" -> Just EndsWith
  "startswith" -> Just StartsWith
  "base64offset" -> Just Base64Offset
  "contains" -> Just Contains
  "all" -> Just AllModifier
  _ -> Nothing

parseAllTokens :: Tokenize.Alex [Internal.Token]
parseAllTokens = do
  t <- Tokenize.alexMonadScan
  case t of
    Internal.TokenEof -> pure [t]
    _ -> do
      ts <- parseAllTokens
      pure (t : ts)

data Search = Search
  { connective :: !Connective
    -- ^ What the Sigma spec calls @{map-list}@ means disjunction, and
    -- what it calls @{field: value}@ means conjunction.
  , matches :: !(Vector Match)
  } 

data Detection = Detection
  { condition :: !Condition
  , searches :: !(Map Text Search)
  }

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
