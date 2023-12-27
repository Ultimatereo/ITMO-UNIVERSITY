#!/bin/bash
echo "" > "report2.log"
echo $$ > .pid2
ind=0
while true
do
	arr[$ind]=$(($ind % 10 + 1))
	if [ $(( $ind % 100000)) -eq 0 ]
	then
		echo $ind >> "report2.log"
	fi
	ind=$(($ind+1))
done