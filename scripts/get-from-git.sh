#! /bin/bash

if [ ! -f DESCRIPTION ]; then
    echo "Run this script from the root of the repo"
    exit 1
fi

wget https://raw.githubusercontent.com/git/git/master/date.c -O \
     src/date.c

patch -p1 < scripts/date.c.patch
