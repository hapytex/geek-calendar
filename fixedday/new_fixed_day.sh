#!/bin/bash

# format date of birthday
dt=$(date '+%Y%m%d-' -d "$1")
shift

# universe
if [ ! -d "$1" ]; then
  echo 'Specify an existing universe directory as second parameter!'
fi
un="$1"
shift

slug=$(bash ../slugify.sh "$@")
dir="$un/$dt$slug"

mkdir -p "$dir"

nm=$(echo "$@")
echo "$nm" >> "$dir/name"

touch "$dir/description.md"
editor -c 'startinsert' "$dir/description.md"

touch "$dir/why.md"
editor -c 'startinsert' "$dir/why.md"

touch "$dir/links"
editor -c 'startinsert' "$dir/links"

touch "$dir/note.md"

git add "$dir/name" "$dir/description.md" "$dir/links" "$dir/why.md" "$dir/note.md"
git commit -am "Add the $nm event"
