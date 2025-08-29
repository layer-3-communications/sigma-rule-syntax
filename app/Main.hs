{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}

import Sigma.Rule (Rule,Modifier(..))
import Data.Set (Set)
import Data.Foldable (foldlM,for_)
import Control.Monad (when)
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.Environment (getArgs,lookupEnv)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.FilePath.Posix (takeBaseName)

import qualified System.IO as IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Sigma.Condition as Condition
import qualified Sigma.Rule.Insight as Insight
import qualified Data.List as List
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text.Lazy as LT

main :: IO ()
main = do
  (subcommand, path) <- getArgs >>= \case
    [subcommand, path] -> pure (subcommand, path)
    _ -> fail "Expected two arguments: subcommand and path"
  case subcommand of
    "summary" -> summarizeDirectory path
    "sql" -> makeSqlPredicates path
    _ -> fail "Unrecognized subcommand, try one of: summary"

data Summary = Summary
  { fieldCounts :: !(Map Text Int) -- how many times each field occurs
  , modifiers :: !(Set (Set Modifier)) -- all modifiers that are used together
  , failures :: !Int -- how many sigma rules could not be decoded
  }

instance Semigroup Summary where
  Summary a1 b1 c1 <> Summary a2 b2 c2 = Summary (Map.unionWith (+) a1 a2) (b1 <> b2) (c1 + c2)

instance Monoid Summary where
  mempty = Summary Map.empty Set.empty 0

summarizeDirectory :: String -> IO ()
summarizeDirectory path = do
  files <- List.sort <$> listDirectory path
  Summary{fieldCounts,modifiers,failures} <- foldlM
    (\acc filename -> do
      if (".yml" `isSuffixOf` filename) || (".yaml" `isSuffixOf` filename)
        then do
          let fullFilename = path ++ "/" ++ filename
          Yaml.decodeFileEither fullFilename >>= \case
            Right (rule :: Rule) -> do
              let fields = Insight.fields rule
              let mods = Insight.modifierSets rule
              putStrLn (filename ++ ": " ++ foldMap (\t -> T.unpack t ++ ", ") fields)
              let counts = Map.fromSet (const (1 :: Int)) fields
              pure (acc <> Summary{fieldCounts=counts,modifiers=mods,failures=0})
            Left err -> do
              IO.hPutStrLn IO.stderr ("Could not decode file " ++ filename ++ ": " ++ show err)
              pure (acc <> Summary{fieldCounts=Map.empty,modifiers=Set.empty,failures=1})
        else pure acc
    ) mempty files
  putStrLn "\nTotal count of each field:" 
  _ <- Map.traverseWithKey
    (\field count -> do
      putStrLn (T.unpack field ++ ": " ++ show count)
    ) fieldCounts
  putStrLn "\nModifier Sets:"
  for_ modifiers $ \mods -> print mods
  putStrLn ("\nCount of rules not decoded: " ++ show failures)
  pure ()

makeSqlPredicates :: String -> IO ()
makeSqlPredicates path = do
  fieldAllowlist <- lookupEnv "SIGMA_FIELD_ALLOWLIST" >>= \case
    Nothing -> pure Set.empty
    Just fields -> pure $ Set.fromList $ T.splitOn (T.singleton ',') (T.pack fields)
  files <- List.sort <$> listDirectory path
  for_ files $ \filename -> when ((".yml" `isSuffixOf` filename) || (".yaml" `isSuffixOf` filename)) $ do
    let fullFilename = path ++ "/" ++ filename
    Yaml.decodeFileEither fullFilename >>= \case
      Right (rule :: Rule) -> case Insight.resolve rule of
        Right cond -> do
          let mods = Insight.modifierSets rule
          let fields = Insight.fields rule
          if | not (Set.null fieldAllowlist), not (fields `Set.isSubsetOf` fieldAllowlist) -> do
                 IO.hPutStrLn IO.stderr ("Skipping " ++ filename ++ ": includes fields not in allowlist")
             | Set.member (Set.fromList [Base64Offset,Contains]) mods -> do
                 IO.hPutStrLn IO.stderr ("Skipping " ++ filename ++ ": base64offset modifier not supported")
             | otherwise -> do
                 let sql = Condition.toSql cond
                 case LT.isAscii sql of
                   True -> do
                     IO.withFile ("generated/" ++ takeBaseName filename ++ ".sql") IO.WriteMode $ \h -> do
                       LTIO.hPutStrLn h sql
                   False -> do
                     IO.hPutStrLn IO.stderr ("Skipping " ++ filename ++ ": SQL would have non-ascii characters")
        Left{} -> fail ("Could not resolve " ++ filename)
      Left _ -> IO.hPutStrLn IO.stderr ("Skipping " ++ filename ++ ": could not decode")
