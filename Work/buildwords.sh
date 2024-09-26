#!/bin/bash

aspell dump master fr \
    | tr '[:lower:]' '[:upper:]' \
    | grep -v '-' \
    | grep -v "'" \
    | iconv -f utf8 -t ascii//TRANSLIT \
    | sed "s/['\`\"^]//g" \
    | sort \
    | uniq \
    > dictionary.txt

