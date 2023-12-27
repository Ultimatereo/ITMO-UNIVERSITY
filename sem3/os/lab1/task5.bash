#!/bin/bash
awk -F' ' '$2=="INFO"' syslog.txt > info.log
cat info.log
