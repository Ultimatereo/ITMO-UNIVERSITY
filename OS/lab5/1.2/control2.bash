#!/bin/bash
echo "" > "report2.txt"
while sleep 15
do
	echo "System memory parameter values:" >> report2.txt
	top -n 1 -b | awk 'NR == 4 { print $4 " -total " $6  " -free " $8 " -used " $10 " -buff/cashe" }' >> report2.txt
	top -n 1 -b | awk 'NR == 5 {print $3 " -total " $5  " -free " $7 " -used " $9 " -buff/cashe"}' >> report2.txt
	echo "" >> report2.txt
	a=$(top -n 1 -b | grep "$(cat .pid2)")
	if [ "$a" = "" ]
	then
		echo "Pid process is not found anymore"
		dmesg | grep "mem" | tail -n 2 >> report2.txt
		break
	fi
	top -n 1 -b | grep "$(cat .pid2)" >> report2.txt
	echo "" >> report2.txt
	top -n 1 -b | awk 'NR>=8' | awk 'NR<=5' >> report2.txt
	echo "" >> report2.txt
done