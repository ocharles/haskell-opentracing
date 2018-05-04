{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.OpenTracingSpec where

import qualified Data.ByteString.Lazy.Char8         as LBS
import           Data.Text.Lazy.Encoding            (encodeUtf8)
import           Jaeger
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.OpenTracing
import           Network.Wai.Test
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "OpenTracing WAI Middleware" $ do

  it "set tracing header on request when it hits middleware" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    let tr = t { tracerIdGenerator = pure 1234 }

    resp <- runSession (request defaultRequest) $ openTracingMiddleware t $ echoHeadersApp

    simpleHeaders resp `shouldBe` [(tracingHeader, "1234")]


echoHeadersApp :: Application
echoHeadersApp req resp =
  let hdrs = requestHeaders req
  in resp $ responseLBS status200 hdrs ""
