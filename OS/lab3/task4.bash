#!/bin/bash
bash task4proc.bash&pid1=$!
bash task4proc.bash&pid2=$!
bash task4proc.bash&pid3=$!

time1=30
time2=30
time=$((time1+time2))

bash task4manager.bash $pid1 $time&
sleep $time1
kill $pid3
echo "pid3 is killed"
sleep $time2


kill $pid1
kill $pid2
echo "everyone is killed"