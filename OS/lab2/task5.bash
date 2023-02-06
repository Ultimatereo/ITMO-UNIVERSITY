#!/bin/bash
bash task4.bash $1 > null

cur_ppid='0'
sum_art=0
counter=0
> tmp2.txt
while read CMD; do
	echo $CMD | awk -F":" '{print $2}' > tmp.txt
	ppid=$(awk -F"=" '{print $2}' tmp.txt | xargs)
	echo $CMD | awk -F":" '{print $3}' > tmp.txt
	art=$(awk -F"=" '{print $2}' tmp.txt | xargs)
	if [[ "$ppid" == "$cur_ppid" ]]; then
		sum_art=$(echo $sum_art $art | awk '{ print $1 + $2 }')
		counter=$((counter + 1));
		echo $CMD >> tmp2.txt
	else
		avg_art=$(echo $sum_art $counter | awk '{print $1 / $2 }')
		echo "Average_Running_Children_of_ParentID=$cur_ppid is $avg_art" >> tmp2.txt
		echo $CMD >> tmp2.txt
		sum_art=$art
		counter=1
		cur_ppid=$ppid 
	fi
done < $1
echo "Average_Running_Children_of_ParentID='$cur_ppid' is '$avg_art'" >> tmp2.txt
cat tmp2.txt > $1
cat $1
