#!/bin/bash

# format date of birthday
dt=$(date '+%Y%m%d-' -d "$1")
shift
slug=$(bash ../slugify.sh "$@")
dir="$dt$slug"

mkdir -p "$dir"

nm=$(echo "$@")
echo "$nm" >> "$dir/name"

touch "$dir/bio.md"
editor -c 'startinsert' "$dir/bio.md"

touch "$dir/links"
editor -c 'startinsert' "$dir/links"

touch "$dir/note.md"

git add "$dir/name" "$dir/bio.md" "$dir/links" "$dir/note.md"
git commit -am "Add birthday of $nm"
