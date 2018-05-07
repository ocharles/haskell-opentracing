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

  it "clears active span when popping span" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    pushSpan t "foo" Nothing  >>= popSpan t

    fmap spanId <$> readActiveSpan t `shouldReturn` Nothing

  it "resets active span to parent span when popping span" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    pushSpan t "bar" Nothing >> pushSpan t "foo" Nothing  >>= popSpan t

    fmap spanOperationName <$> readActiveSpan t `shouldReturn` Just "bar"

  it "sets span parent to currently active span when pushing span given active span exists" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    parent <- do
      pushSpan t "bar" Nothing
      parent <- readActiveSpan t
      pushSpan t "foo" Nothing
      pure parent

    fmap spanOperationName <$> readActiveSpan t `shouldReturn` Just "foo"
    (>>= spanParent) <$> readActiveSpan t `shouldReturn` parent

  it "does not set parent span when pushing span given active span is Nothing" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    pushSpan t "foo" Nothing

    (>>= spanParent) <$> readActiveSpan t `shouldReturn` Nothing
