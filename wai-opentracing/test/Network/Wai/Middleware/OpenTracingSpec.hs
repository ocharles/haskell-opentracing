{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.OpenTracingSpec where

import qualified Data.ByteString.Lazy.Char8         as LBS
import           Data.IORef
import           Data.Maybe
import           Data.Text.Lazy.Encoding            (encodeUtf8)
import           Jaeger
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.OpenTracing
import           Network.Wai.Test
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "OpenTracing WAI Middleware" $ do

  it "set tracing header on request given no header is set" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    let tr = t { tracerIdGenerator = pure 1234 }

    resp <- runSession (request defaultRequest) $ openTracingMiddleware tr $ echoHeadersApp tr

    simpleHeaders resp `shouldBe` [(tracingHeader, "1234")]

  it "replace tracing header on request and use previous value as parent id given tracing header is set" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    let tr = t { tracerIdGenerator = pure 1234 }

    let req = defaultRequest { requestHeaders = [ (tracingHeader, "4567") ] }

    resp <- runSession (request req) $ openTracingMiddleware tr $ echoHeadersApp tr

    simpleHeaders resp `shouldBe` [(tracingHeader, "1234")]
    simpleBody resp `shouldBe` "4567"


echoHeadersApp :: Tracer -> Application
echoHeadersApp Tracer{tracerActiveSpan} req resp =
  let hdrs = requestHeaders req
  in do
    sp <- maybe "" (show . spanTraceId) <$> readIORef tracerActiveSpan
    resp $ responseLBS status200 hdrs (LBS.pack sp)
