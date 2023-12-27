#!/bin/bash
man bash | grep -o "[a-zA-Z]\{4,\}" | tr "[:upper:]" "[:lower:]" | sort | uniq -c | sort -nr | head -n3
