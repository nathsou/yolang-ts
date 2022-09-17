#!/bin/bash

# see https://evacchi.github.io/llvm/wasm/wasi/2022/04/14/compiling-llvm-ir-into-wasm.html

BUILD_DIR="./build"
WASI_DIR="wasi"
WASI_SDK_URL="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-16/wasi-sysroot-16.0.tar.gz"
WASI_LIBCLANG_URL="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-16/libclang_rt.builtins-wasm32-wasi-16.0.tar.gz"

mkdir -p $BUILD_DIR
cd $BUILD_DIR
mkdir $WASI_DIR

curl -LO $WASI_SDK_URL
tar xzvf wasi-sysroot-16.0.tar.gz
rm wasi-sysroot-16.0.tar.gz
mv wasi-sysroot $WASI_DIR/wasi-sysroot

curl -LO $WASI_LIBCLANG_URL
tar xzvf libclang_rt.builtins-wasm32-wasi-16.0.tar.gz
rm libclang_rt.builtins-wasm32-wasi-16.0.tar.gz
mv lib $WASI_DIR/libclang
rm -rf lib
