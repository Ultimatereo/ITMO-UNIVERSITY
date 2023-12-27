#!/bin/bash
echo "" > full.log
awk -F' ' '$3=="(WW)"' X.txt >> full.log
awk -F' ' '$3=="(II)"' X.txt >> full.log
sed -i 's/(WW)/Warning/g' full.log
sed -i 's/(II)/Information/g' full.log
cat full.log
