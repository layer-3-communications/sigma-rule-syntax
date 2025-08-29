{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

module Sigma.Rule.Insight
  ( fields
  , modifierSets
  , resolve
  ) where

import Sigma.Rule

import Data.Text (Text)
import Data.Set (Set)
import Data.Text (pattern (:>))
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

fields :: Rule -> Set Text
fields Rule{detection=Detection{searches}} = foldMap
  (\Search{matches} -> foldMap
    (\Match{field} -> Set.singleton field
    ) matches
  ) searches

modifierSets :: Rule -> Set (Set Modifier)
modifierSets Rule{detection=Detection{searches}} = foldMap
  (\Search{matches} -> foldMap
    (\Match{modifiers=m} -> Set.singleton m
    ) matches
  ) searches

-- | Convert the detection into a usable query with all of the rules resolved.
resolve :: Rule -> Either () (Condition 'Resolved)
resolve Rule{detection=Detection{condition,searches}} =
  resolveCondition searches condition

resolveCondition ::
     Map Text Search
  -> Condition 'Unresolved
  -> Either () (Condition 'Resolved)
resolveCondition ctx = \case
  And a b -> liftA2 And (resolveCondition ctx a) (resolveCondition ctx b)
  Or a b -> liftA2 Or (resolveCondition ctx a) (resolveCondition ctx b)
  Not a -> fmap Not (resolveCondition ctx a)
  Of conn pat -> resolveSearchPattern ctx conn pat
  Reference name -> case Map.lookup name ctx of
    Nothing -> Left ()
    Just s -> Right (ResolvedReference s)

resolveSearchPattern ::
     Map Text Search
  -> Connective
  -> SearchPattern
  -> Either () (Condition 'Resolved)
resolveSearchPattern ctx conn pat = case pat of
  SearchExact name -> case Map.lookup name ctx of
    Nothing -> Left ()
    Just s -> Right (ResolvedReference s)
  SearchPrefix pre -> case pre of
    pre0 :> c -> do
      let post = pre0 :> succ c
      let (_,gtePre) = Map.split pre ctx
      let (ltPost,_) = Map.split post gtePre
      pure $! ResolvedOf conn (V.fromList (Map.elems ltPost))
    _ -> Right $ ResolvedOf conn (V.fromList (Map.elems ctx))
