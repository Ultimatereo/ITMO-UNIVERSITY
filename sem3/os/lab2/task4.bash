#!/bin/bash
> $1
for pid in $(ps uax | awk 'FNR > 1 { print $2 }'); do
    if [ -r /proc/$pid/ ] && [ -r /proc/$pid/status ] && [ -r /proc/$pid/sched ]; then	
        ppid=$(grep "PPid" "/proc/$pid/status" | awk '{ print $2 }')
        sum_exec_runtime=$(grep "sum_exec_runtime" "/proc/$pid/sched" | awk '{ print $3 }')
        nr_switches=$(grep "nr_switches" "/proc/$pid/sched" | awk '{ print $3 }')
        art=$(echo "$sum_exec_runtime  $nr_switches" | awk '{ print $1 / $2 }')
        echo "$pid $ppid $art" >> $1
    fi
done
echo "$(sort "$1" -nk2 | awk '{ print "ProcessID="$1" : Parent_ProcessID="$2" : Average_Running_Time="$3 }')" > $1
column -t $1
