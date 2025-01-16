#!/usr/bin/env bash

if [[ $PWD != */web ]]; then
    echo "This script is meant to be run in the 'web' directory"
    exit 1
fi

wasm32-wasi-cabal build xolsh-exe && wasmpath=$(wasm32-wasi-cabal list-bin xolsh-exe)
cp -f $wasmpath dist/xolsh-exe.wasm

cp -R static/ dist/

esbuild ./src/index.js --bundle --serve=127.0.0.1:9000 --servedir=./dist --outfile=./dist/index.js --global-name=Main --format=esm --platform=browser "--external:node:timers"