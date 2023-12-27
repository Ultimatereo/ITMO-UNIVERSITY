#!/bin/bash
echo "" > "report.txt"
while sleep 15
do
	echo "System memory parameter values:" >> report.txt
	top -n 1 -b | awk 'NR == 4 { print $4 " -total " $6  " -free " $8 " -used " $10 " -buff/cashe" }' >> report.txt
	top -n 1 -b | awk 'NR == 5 {print $3 " -total " $5  " -free " $7 " -used " $9 " -buff/cashe"}' >> report.txt
	echo "" >> report.txt
	a=$(top -n 1 -b | grep "$(cat .pid)")
	if [ "$a" = "" ]
	then
		echo "Pid process is not found anymore"
		dmesg | grep "mem" | tail -n 2 >> report.txt
		break
	fi
	top -n 1 -b | grep "$(cat .pid)" >> report.txt
	echo "" >> report.txt
	top -n 1 -b | awk 'NR>=8' | awk 'NR<=5' >> report.txt
	echo "" >> report.txt
done