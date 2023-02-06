#!/bin/bash
echo -e "$1\n$2\n$3" | sort -n | tail -1
echo $@ | tr ' ' '\n' | sort -n | tail -1
