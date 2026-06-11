#!/bin/bash
set -e

PREFIX=/opt/local   # or /opt/homebrew

# 1. Compile C++ wrapper
clang++ -std=c++20 -stdlib=libc++ \
    -I$PREFIX/include \
    -c cpp/parquet_wrapper.cpp \
    -o parquet_wrapper.o

# 2. Build Fortran objects (no linking)
fpm build

# 3. Find compiled objects
OBJ_DIR=$(find build -type d -name "*gfortran*")

# 4. Link EVERYTHING together
gfortran \
    $OBJ_DIR/app/*.o \
    $OBJ_DIR/src/*.o \
    parquet_wrapper.o \
    -L$PREFIX/lib \
    -larrow -lparquet -lstdc++ \
    -o run_parquet_fortran
