#!/bin/bash
awk -F':' '{print $1, $3}' "/etc/passwd" | sort -nk2 | column -t
