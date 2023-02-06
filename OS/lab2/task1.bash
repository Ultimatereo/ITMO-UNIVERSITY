#!/bin/bash
user="$(whoami)"
user=${user:0:7}"+"
a="$(ps ua | awk -v var="$user" '{ if ($1==var) print $0 }')"
echo "$a" | wc -l > $1
echo "$a" | awk '{ print $2, $11 }' | column -t >> $1
cat $1
