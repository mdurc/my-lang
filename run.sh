#!/bin/bash

g++-14 \
src/*.cpp \
-o compiler

./compiler $1
