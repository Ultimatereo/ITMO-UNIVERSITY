#!/bin/bash
echo "" > "report.log"
echo $$ > .pid
ind=0
while true
do
	arr[$ind]=$(($ind % 10 + 1))
	if [ $(( $ind % 100000)) -eq 0 ]
	then
		echo $ind >> "report.log"
	fi
	ind=$(($ind+1))
done