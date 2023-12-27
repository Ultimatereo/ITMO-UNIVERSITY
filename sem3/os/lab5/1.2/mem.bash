#!/bin/bash
echo "" > "report1.log"
echo $$ > .pid1
ind=0
while true
do
	arr[$ind]=$(($ind % 10 + 1))
	if [ $(( $ind % 100000)) -eq 0 ]
	then
		echo $ind >> "report1.log"
	fi
	ind=$(($ind+1))
done