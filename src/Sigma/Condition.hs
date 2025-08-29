{-# language LambdaCase #-}
{-# language DataKinds #-}
{-# language OverloadedStrings #-}

module Sigma.Condition
  ( toSql
  ) where

import Sigma.Rule

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy as LT

-- This is responsible for adding the parens.
toSql :: Condition 'Resolved -> LT.Text
toSql = \case
  And a b -> "(" <> toSql a <> " AND " <> toSql b <> ")"
  Or a b -> "(" <> toSql a <> " OR " <> toSql b <> ")"
  Not a -> "(NOT (" <> toSql a <> "))"
  ResolvedReference s -> searchToSql s
  ResolvedOf conn s -> searchesToSql conn s

searchesToSql :: Connective -> Vector Search -> LT.Text
searchesToSql conn s = case V.uncons s of
  Nothing -> case conn of
    Conjunction -> "(1=1)"
    Disjunction -> "(1=0)"
  Just (hd,tl) -> foldl' (\acc search -> acc <> connectiveToKeyword conn <> searchToSql search) (searchToSql hd) tl

searchToSql :: Search -> LT.Text
searchToSql Search{connective,matches} = case V.uncons matches of
  Nothing -> case connective of
    Conjunction -> "(1=1)"
    Disjunction -> "(1=0)"
  Just (hd,tl) ->
    let p = foldl' (\acc match -> acc <> connectiveToKeyword connective <> matchToSql match) (matchToSql hd) tl
     in case V.length tl of
          0 -> p
          _ -> "(" <> p <> ")"

connectiveToKeyword :: Connective -> LT.Text
connectiveToKeyword = \case
  Conjunction -> " AND "
  Disjunction -> " OR "

matchToSql :: Match -> LT.Text
matchToSql Match{field,modifiers,values}
  | modifiers == Set.empty = case V.length values of
      0 -> "(1=0)"
      1 -> let value = V.head values in case value of
        Null -> fieldSql <> " IS NULL"
        _ -> fieldSql <> " = " <> literalToSqlLit value
      _ ->
        let listBody = foldl' (\acc lit -> acc <> ", " <> literalToSqlLit lit) (literalToSqlLit (V.head values)) (V.tail values)
         in fieldSql <> " IN (" <> listBody <> ")"
  | modifiers == Set.singleton StartsWith = case V.uncons values of
      Nothing -> "(1=0)"
      Just (hd,tl) ->
        "("
        <>
        foldl'
          (\acc suf -> acc <> " OR " <> makeStartsWith field suf
          ) (makeStartsWith field hd) tl
        <>
        ")"
  | modifiers == Set.singleton EndsWith = case V.uncons values of
      Nothing -> "(1=0)"
      Just (hd,tl) ->
        "("
        <>
        foldl'
          (\acc suf -> acc <> " OR " <> makeEndsWith field suf
          ) (makeEndsWith field hd) tl
        <>
        ")"
  | modifiers == Set.singleton Contains = case V.uncons values of
      Nothing -> "(1=0)"
      Just (hd,tl) ->
        "("
        <>
        foldl'
          (\acc suf -> acc <> " OR " <> makeContains field suf
          ) (makeContains field hd) tl
        <>
        ")"
  | modifiers == Set.fromList [Contains, AllModifier] = case V.uncons values of
      Nothing -> "(1=0)"
      Just (hd,tl) ->
        "("
        <>
        foldl'
          (\acc suf -> acc <> " AND " <> makeContains field suf
          ) (makeContains field hd) tl
        <>
        ")"
  where
  fieldSql = escapeField field

escapeField :: Text -> LT.Text
escapeField field = "\"" <> LT.fromStrict field <> "\""

-- This does not put quotes around the string
escapeString :: Text -> LT.Text
escapeString = LT.fromStrict . T.replace "'" "''"

makeEndsWith ::
     Text -- field name
  -> Literal -- suffix
  -> LT.Text
makeEndsWith field suffix = fieldSql <> " LIKE '%" <> litAfterPercent <> "'"
  where
  fieldSql = escapeField field
  litAfterPercent = case suffix of
    String s -> escapeString s
    Number n -> LT.pack (show n)

makeStartsWith ::
     Text -- field name
  -> Literal -- suffix
  -> LT.Text
makeStartsWith field suffix = fieldSql <> " LIKE '" <> litBeforePercent <> "%'"
  where
  fieldSql = escapeField field
  litBeforePercent = case suffix of
    String s -> escapeString s
    Number n -> LT.pack (show n)

makeContains ::
     Text -- field name
  -> Literal -- suffix
  -> LT.Text
makeContains field suffix = fieldSql <> " LIKE '%" <> litAfterPercent <> "%'"
  where
  fieldSql = escapeField field
  litAfterPercent = case suffix of
    String s -> escapeString s
    Number n -> LT.pack (show n)

literalToSqlLit :: Literal -> LT.Text
literalToSqlLit = \case
  String s -> "'" <> escapeString s <> "'"
  Number n -> LT.pack (show n)
