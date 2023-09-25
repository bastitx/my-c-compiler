#!/bin/bash

/home/bastian/Development/my_compiler/target/debug/my_compiler $1
f=$1
s_file="${f%.c}.s"
# o_file="${f%.c}.o"
file="${f%.c}"
gcc $s_file -o $file
