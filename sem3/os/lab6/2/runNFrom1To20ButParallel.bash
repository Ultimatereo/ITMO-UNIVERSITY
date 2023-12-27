#!/bin/bash
# K number of runs
N=20
report="reportPar2.txt"
echo "" > $report
for run in $(seq $N); do
	echo "A new sequence for run: $run" >> $report 
	for run2 in $(seq 6); do
		bash createFiles.bash $run
		{ time bash parallelRun.bash $run; } 2>> $report
	done
done