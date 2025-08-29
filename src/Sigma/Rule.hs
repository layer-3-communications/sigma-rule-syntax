{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedRecordDot #-}
{-# language OverloadedStrings #-}

module Sigma.Rule
  ( Rule(..)
  , Status(..)
  , Logsource(..)
  , Modifier(..)
  , Literal(..)
  , Match(..)
  , Detection(..)
  , Condition(..)
  , Resolution(..)
  , Connective(..)
  , SearchPattern(..)
  , Search(..)
  , Level(..)
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
import Sigma.Rule.Internal (Condition(..),SearchPattern(..),Connective(..),Resolution(..))
import Sigma.Rule.Internal (Modifier(..),Literal(..),Search(..),Match(..))
import Sigma.Rule.Internal (parseSearch)

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
    , "level" .= r.level
    ]

instance ToJSON Level where
  toJSON = \case
    Informational -> Aeson.String "informational"
    Low -> Aeson.String "low"
    Medium -> Aeson.String "medium"
    High -> Aeson.String "high"
    Critical -> Aeson.String "critical"

instance FromJSON Level where
  parseJSON = Aeson.withText "Level" $ \m -> case m of
    "informational" -> pure Informational
    "low" -> pure Low
    "medium" -> pure Medium
    "high" -> pure High
    "critical" -> pure Critical
    _ -> fail "unrecognized level"

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
    level <- m .:? "level" >>= \case
      Nothing -> pure Informational
      Just v -> pure v
    pure Rule{title,id,status,description,author,date,modified,logsource,detection,level}

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

-- Sigma expects dates to be formatted as YYYY/mm/dd or maybe YYYY-mm-dd
parserDate :: Text -> Aeson.Parser Chronos.Date
parserDate t =
  case Atto.parseOnly (Chronos.parser_Ymd (Just '/') <* Atto.endOfInput) t of
    Left _ -> case Atto.parseOnly (Chronos.parser_Ymd (Just '-') <* Atto.endOfInput) t of
      Left e -> fail e
      Right d -> pure d
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
  , level :: !Level
  }

-- | Criticality of a triggered rule
data Level
  = Informational
  | Low
  | Medium
  | High
  | Critical

instance FromJSON Detection where
  parseJSON = Aeson.withObject "Detection" $ \m -> do
    condition <- (m .: "condition") >>= parseCondition
    let m1 = KeyMap.toMapText m
    searches <- traverse parseSearch (M.delete "condition" m1)
    pure Detection
      { condition
      , searches
      }

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

encodeCondition :: Int -> Condition 'Unresolved -> LT.Text
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
  Reference s -> LT.fromStrict s
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

parseCondition :: Aeson.Value -> Aeson.Parser (Condition 'Unresolved)
parseCondition = Aeson.withText "Condition" $ \t ->
  case decodeCondition t of
    Left e -> fail e
    Right c -> pure c


decodeCondition :: Text -> Either String (Condition 'Unresolved)
decodeCondition t = do
  tokens <- Tokenize.runAlex (LBS.fromStrict (TE.encodeUtf8 t)) parseAllTokens
  first
    (\e -> e ++ ", tokenized: " ++ show tokens)
    (Parser.parseSigma tokens)

parseAllTokens :: Tokenize.Alex [Internal.Token]
parseAllTokens = do
  t <- Tokenize.alexMonadScan
  case t of
    Internal.TokenEof -> pure [t]
    _ -> do
      ts <- parseAllTokens
      pure (t : ts)

data Detection = Detection
  { condition :: !(Condition 'Unresolved)
  , searches :: !(Map Text Search)
  }
