#!/bin/bash

aspell dump master fr \
    | iconv -f utf8 -t ascii//TRANSLIT//IGNORE \
    | tr '[:lower:]' '[:upper:]' \
    | grep -v '-' \
    | tr -d "^'\`\"" \
    | grep -v '^.$' \
    | tee >( sort > dictionary.txt )

