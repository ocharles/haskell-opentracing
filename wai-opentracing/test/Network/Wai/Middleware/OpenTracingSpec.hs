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

    let tr = t { tracerIdGenerator = pure 1234567890 }

    resp <- runSession (request defaultRequest) $ openTracingMiddleware tr $ echoHeadersApp tr

    simpleHeaders resp `shouldBe` [(tracingHeader, "00000000499602d2:00000000499602d2:00:1")]

  it "replace tracing header on request and use previous value as parent id given tracing header is set" $ do
    t <- openTracer (TracerConfiguration "localhost" "9999" "service")

    let tr = t { tracerIdGenerator = pure 4567890123 }

    let req = defaultRequest { requestHeaders = [ (tracingHeader, "00000000499602d2:00000000499602d2:00:1") ] }

    resp <- runSession (request req) $ openTracingMiddleware tr $ echoHeadersApp tr

    simpleHeaders resp `shouldBe` [(tracingHeader, "00000000499602d2:00000001104478cb:00:1")]
    simpleBody resp `shouldBe` "1234567890"


echoHeadersApp :: Tracer -> Application
echoHeadersApp tracer req resp =
  let hdrs = requestHeaders req
  in do
    sp <- maybe "" (show . spanTraceId) <$> readActiveSpan tracer
    resp $ responseLBS status200 hdrs (LBS.pack sp)
