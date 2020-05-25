#!/bin/bash

# format date of birthday
dt=$(date '+%Y%m%d-' -d "$1")
shift
slug=$(bash ../slugify.sh "$@")
dir=$(echo "$dt$slug")

mkdir "$dt$slug"

echo "$@" >> "$dir/name"

touch "$dir/bio.md"
editor "$dir/bio.md"

touch "$dir/links"
editor "$dir/links"

touch "$dir/note.md"

git add "$dir/name" "$dir/bio.md" "$dir/links" "$dir/note.md"
