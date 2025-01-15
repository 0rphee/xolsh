#!/usr/bin/env bash

if [[ $PWD != */web ]]; then
    echo "This script is meant to be run in the 'web' directory"
    exit 1
fi

wasm32-wasi-cabal build && wasmpath=$(wasm32-wasi-cabal list-bin xolsh-web)

$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $wasmpath -o ./dist/ghc_wasm_jsffi.js

cp -f $wasmpath dist/xolsh-web.wasm
cp -f static/index.html dist/index.html

esbuild ./src/index.js --bundle --serve=127.0.0.1:9000 --servedir=./dist --outfile=./dist/index.js --global-name=Main --format=esm --platform=browser "--external:node:timers"