#!/usr/bin/env bash

FOLDER=${1:-'../../examples'}
if [ ! -d $FOLDER ]; then
    echo "$FOLDER is not a directory."
    exit 1
fi
for file in ${FOLDER}/*.cl
do
    echo Testing ${file}:
    diff --suppress-common-lines -y -s <(lexer $file | parser | semant | cgen) <(lexer $file | parser | semant | ./cgen)
done