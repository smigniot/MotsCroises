#!/bin/bash

aspell dump master fr \
    | iconv -f utf8 -t ascii//TRANSLIT//IGNORE \
    | tr '[:lower:]' '[:upper:]' \
    | grep -v '-' \
    | grep -v "'" \
    | tr -d "^'\`\"" \
    | grep -v '^.$' \
    | tee >( sort | uniq > dictionary.txt )

