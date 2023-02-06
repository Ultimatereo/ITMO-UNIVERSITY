#!/bin/bash
# K number of runs
N=$1
for run in $(seq $N); do
	bash double.bash $run&pids[$run]=$!
done

for pid in ${pids[*]}; do
	wait $pid
done