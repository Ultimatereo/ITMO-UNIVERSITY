#!/bin/bash
ps uax | awk '{ print $2, $11 }' | grep "/init" | column -t > $1
cat $1
