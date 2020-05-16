#!/bin/bash

# format date of birthday
dt=$(date '+%Y%m%d-' -d "$1")
shift
slug=$(bash ../slugify.sh "$@")
dir=$(echo "$dt$slug")

mkdir "$dt$slug"
echo "$@" >> "$dir/name"
touch "$dir/bio"
editor "$dir/bio"
