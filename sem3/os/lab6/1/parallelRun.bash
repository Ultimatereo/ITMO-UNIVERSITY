#!/bin/bash
# K number of runs
N=$1
for run in $(seq $N); do
	b=$(expr $run \* 100)
	bash mem.bash $run $b&pids[$run]=$!
done

for pid in ${pids[*]}; do
	wait $pid
done