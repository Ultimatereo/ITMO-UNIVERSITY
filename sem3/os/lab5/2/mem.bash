#!/bin/bash
# Принимает единственный параметр N это максимальный размер массива
report=$3
ind=0
while true
do
	arr[$ind]=$(($ind % 10 + 1))
	if [ $ind -eq $1 ]
	then
		echo "Proccess $2 is done" >> $report
		break
	fi
	ind=$(( $ind+1 ))
done