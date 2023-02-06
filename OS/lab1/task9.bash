#!/bin/bash
cat $(find "/var/log" -name '*.log') > bb.txt
wc -l bb.txt
