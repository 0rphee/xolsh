#!/usr/bin/env bash

if [[ $PWD != */web ]]; then
    echo "This script is meant to be run in the 'web' directory"
    exit 1
fi

outdir=""
case $1 in
    dev) 
        echo "dev run"
        outdir="./static/"
        ;;
    prod)
        echo "prod run"
        wasm32-wasi-cabal update
        npm install
        outdir="./dist/"
        cp -R static/ $outdir/
        ;;
    *)
        echo "This script should be invoked with 'dev' or 'prod' as arguments."
        exit 1
        ;;
esac

wasm32-wasi-cabal build xolsh-exe && wasmpath=$(wasm32-wasi-cabal list-bin xolsh-exe)

cp -f $wasmpath $outdir/xolsh-exe.wasm

case $1 in
    dev)
        esbuild ./src/index.js --bundle --serve=127.0.0.1:9000 --servedir=./static --outfile=./static/index.js --format=esm --platform=browser "--external:node:timers"
        ;;
    prod)
        esbuild ./src/index.js --bundle --outfile=./dist/index.js --format=esm --platform=browser "--external:node:timers"
        ;;
esac
