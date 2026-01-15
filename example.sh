#!/bin/sh

bin=$( cabal exec -- which hs-http-path-checker )

echo /path/example/ok | "${bin}"
echo path/example/ng | "${bin}"
