#!/bin/bash
ind=$1
step=$2
end=$(expr $1 + $2 \* 5000)
while true
do
	a=$(expr $a + 1)
	if [ $ind -gt $end ]
	then
		break
	fi
	ind=$(($ind + $step))
done