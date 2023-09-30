#!/bin/bash

PROJECT_DIR="$(dirname "$(readlink -f "$0")")"
${PROJECT_DIR}/target/release/my_compiler $1
f=$1
s_file="${f%.c}.s"
# o_file="${f%.c}.o"
file="${f%.c}"
gcc $s_file -o $file
rm $s_file
