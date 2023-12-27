#!/bin/bash
echo "" > "report1.txt"
while sleep 15
do
	echo "System memory parameter values:" >> report1.txt
	top -n 1 -b | awk 'NR == 4 { print $4 " -total " $6  " -free " $8 " -used " $10 " -buff/cashe" }' >> report1.txt
	top -n 1 -b | awk 'NR == 5 {print $3 " -total " $5  " -free " $7 " -used " $9 " -buff/cashe"}' >> report1.txt
	echo "" >> report1.txt
	a=$(top -n 1 -b | grep "$(cat .pid1)")
	if [ "$a" = "" ]
	then
		echo "Pid process is not found anymore"
		dmesg | grep "mem" | tail -n 2 >> report1.txt
		break
	fi
	top -n 1 -b | grep "$(cat .pid1)" >> report1.txt
	echo "" >> report1.txt
	top -n 1 -b | awk 'NR>=8' | awk 'NR<=5' >> report1.txt
	echo "" >> report1.txt
done