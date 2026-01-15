FROM haskell:9.12.2-slim-bookworm AS builder
RUN echo cabal update date: 2026-01-05
RUN cabal update --verbose=2

WORKDIR /hs-http-path-checker
COPY --link ./hs-http-path-checker.cabal ./
RUN cabal update --verbose=2
RUN cabal build --only-dependencies
COPY --link ./app/ ./app/
COPY --link ./src/ ./src/

RUN cabal build
RUN cp $( cabal list-bin hs-http-path-checker | fgrep --max-count=1 hs-http-path-checker ) /usr/local/bin/
RUN which hs-http-path-checker
RUN echo /path/example/ok | hs-http-path-checker
RUN echo path/example/ng | hs-http-path-checker


FROM debian:bookworm-slim
COPY --link --from=builder /usr/local/bin/hs-http-path-checker /usr/local/bin/

CMD ["/usr/local/bin/hs-http-path-checker"]
