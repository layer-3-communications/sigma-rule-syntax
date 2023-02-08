{-# language BangPatterns #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language DataKinds #-}

import Prelude hiding (replicate)

import Sigma.Rule
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.Golden (goldenVsString,findByExtension)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.List as List
import qualified Data.Yaml as Yaml

main :: IO ()
main = do
  normalizeTests <- buildNormalizeTests
  defaultMain $ testGroup "tests"
    [ testGroup "normalize" normalizeTests
    ]

buildNormalizeTests :: IO [TestTree]
buildNormalizeTests = do
  names0 <- findByExtension [".yml"] "golden/normalize"
  let names = List.sort names0
  pure $ flip map names $ \fileName -> goldenVsString
    (List.takeWhile (/= '.') fileName)
    (inputToOutput fileName)
    (do rule :: Rule <- Yaml.decodeFileThrow fileName
        pure (encodePretty rule)
    )

-- Convert the filename of the input to the filename where the expected
-- output lives. (e.g. foo.in.txt ==> foo.out.txt)
inputToOutput :: String -> String
inputToOutput s = takeWhile (/= '.') s ++ ".json"
