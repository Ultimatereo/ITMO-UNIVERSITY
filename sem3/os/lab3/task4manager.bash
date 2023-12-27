#!/bin/bash
max_cpu=10
time=$2
end=$((SECONDS+time))
while [ $SECONDS -lt $end ]; do
	sleep 2
	cpu=$(ps -p $1 -o pcpu | tail -n 1)
	echo Used CPU of the first process: $cpu
	if (( $(echo "$max_cpu < $cpu" | bc -l) ))
	then
		nice=$(ps -p $1 -o ni | tail -n 1)
		if [[ $nice -le 19 ]]
		then		
			nice=$(($nice+1))
		fi
		echo Nice of the first process: $nice
		renice -n $nice -p $1 >> /dev/null
	fi 
done 