#!/bin/bash

L=${1:-fr}
aspell dump master $L \
    | tr '[:lower:]' '[:upper:]' \
    | grep -v '-' \
    | grep -v "'" \
    | iconv -f utf8 -t ascii//TRANSLIT \
    | sed "s/['\`\"^]//g" \
    | sort \
    | uniq \
    > dictionary_$L.txt

