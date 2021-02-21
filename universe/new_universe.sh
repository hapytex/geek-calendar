#!/bin/bash

# slugify the name of the universe
slug=$(bash ../slugify.sh "$@")
dir=$(echo "$slug")

mkdir -p "$dir"

nm=$(echo "$@")
echo "$nm" >> "$dir/name"

touch "$dir/emoji"
editor -c 'startinsert' "$dir/emoji"

touch "$dir/links"
editor -c 'startinsert' "$dir/links"

git add "$dir/name" "$dir/emoji" "$dir/links"
git commit -am "added the $nm universe"
