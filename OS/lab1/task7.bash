#!/bin/bash
grep -Eiorhsa '([[:alnum:]_.-]+@[[:alnum:]_.-]+?\.[[:alpha:].]{2,6})' "/etc" | sort | uniq > emails.lst
cat emails.lst
