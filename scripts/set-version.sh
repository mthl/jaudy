#!/bin/sh

set -e -u -x

# Update child deps
sed -i "s/\[fr.reuz\/jaudy\(.*\) \".*\"\]/[fr.reuz\/jaudy\1 \"$1\"\]/g" project.clj

# Update child projects
find . -name project.clj \
       -exec sed -i "s/defproject fr.reuz\/jaudy\(.*\) \".*\"/defproject fr.reuz\/jaudy\1 \"$1\"/g" '{}' \;
