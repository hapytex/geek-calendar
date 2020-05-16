#!/bin/bash

# source: https://gist.github.com/oneohthree/f528c7ae1e701ad990e6
echo "$@" | iconv -t ascii//TRANSLIT | sed -r 's/[^a-zA-Z0-9]+/_/g' | sed -r 's/^-+\|-+$//g' | tr A-Z a-z
