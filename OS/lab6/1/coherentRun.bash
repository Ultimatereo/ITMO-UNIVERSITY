#!/bin/bash
# K number of runs
N=$1
for run in $(seq $N); do
	# echo "hello $run"
	b=$(expr $run \* 100)
	bash mem.bash $run $b
done