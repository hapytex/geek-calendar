#!/bin/bash

# slugify the name of the universe
slug=$(bash ../slugify.sh "$@")
dir="$slug"

# create a directory and create a directory for calendar events
mkdir -p "$dir" "../fixedday/$dir"

nm=$(echo "$@")
echo "$nm" >> "$dir/name"

touch "$dir/emoji"
editor -c 'startinsert' "$dir/emoji"

touch "$dir/links"
editor -c 'startinsert' "$dir/links"

git add "$dir/name" "$dir/emoji" "$dir/links"
git commit -am "added the $nm universe"
