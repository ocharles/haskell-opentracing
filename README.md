# Haskell & OpenTracing

This repository contains a Haskell ecosystem for the OpenTracing standard. In
particular, we have

* `opentracing-client`: A partial module for the OpenTracing standard (using
  Backpack).

* `jaeger-client`: An implementation of `opentracing-client` that sends tracing
  to a Jaeger agent.

* `test-lib`: An example library that uses `opentracing-client`.

* `test-exe`: An example application that uses `test-lib` and combines it with
  `jaeger-client`.
