#!/bin/bash

f="$1"
base=`basename "$f"`
n="${base%.*}"

sed 's/^>/    >/' "$f" | python gendoc.py \
    --stylesheet=style.css \
    --link-stylesheet > "${n}.html"
