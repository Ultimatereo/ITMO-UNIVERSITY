#!/bin/bash
max="0"
for pid in $(ps uax | awk 'FNR > 1 { print $2 } '); do	
    if [[ -r /proc/$pid/status ]]; then
		a="$(grep -i "VmRSS" /proc/$pid/status | awk '{ print $2 }')"
		if [[ "$a" -gt "$max" ]]; then
            ans=$pid
	        max=$a
		fi
    fi
done
echo "PID : $ans Memory : $max"
echo " "
ps uax > tmp.txt
cat tmp.txt
echo " "
ans1="$(cat tmp.txt | awk -v var=$ans '$2 == var {print $1}')" 
ans2="$(cat tmp.txt | awk 'FNR>1 {print $1}' | sort | uniq -c | sort -nr | head -n1 | awk '{print $2}')"

if [ "$ans1" == "$ans2" ]; then
	echo "They are the same : $ans1"
else
	echo "They are different : $ans1 and $ans2"
fi
# mod 
# Сравнить пользователя который запустил этот процесс, который жрёт больше памяти.
# И запустить с пользователем, который запустил больше всего процессов.
# Тот же ли этот пользователь?