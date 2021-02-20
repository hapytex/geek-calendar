#!/bin/bash

# slugify the name of the universe
slug=$(bash ../slugify.sh "$@")
dir=$(echo "$slug")

mkdir -p "$dir"

echo "$@" >> "$dir/name"

touch "$dir/emoji"
editor -c 'startinsert' "$dir/emoji"

touch "$dir/links"
editor -c 'startinsert' "$dir/links"

git add "$dir/name" "$dir/emoji" "$dir/links"
