{-# LANGUAGE OverloadedStrings #-}
module JaegerSpec where

import           Control.Concurrent.Async
import           Data.List
import           Data.Maybe
import           Jaeger
import           Test.Hspec

spec :: Spec
spec = describe "Jaeger Client" $ do

  it "has at most one active span per thread" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    spans <- catMaybes <$> replicateConcurrently 20 (inSpan t "foo" Nothing $ fmap spanId <$> readActiveSpan t)

    length spans `shouldBe` 20
    length (nub spans) `shouldBe` 20
