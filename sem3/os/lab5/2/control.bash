#!/bin/bash
# N - limit of size array
# K number of runs
N=1200000
K=30
report="report30.txt"
for run in $(seq $K); do
	# echo "hello $run"
	sleep 1
	bash mem.bash $N $run $report&
done