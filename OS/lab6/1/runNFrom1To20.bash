#!/bin/bash
# K number of runs
N=20
report="report11.txt"
echo "" > $report
for run in $(seq $N); do
	echo "A new sequence for run: $run" >> $report 
	for run2 in $(seq 6); do
		{ time bash coherentRun.bash $run; } 2>> $report
	done
done