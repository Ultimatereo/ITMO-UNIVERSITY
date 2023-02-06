#!/bin/bash
awk '$3 >= 90 {print $1, $2, "5"}' "students.txt" | sort -nrk3 > bigMen.txt
awk '90 > $3 && $3 >= 75 {print $1, $2, "4"}' "students.txt" | sort -rnk3 >> bigMen.txt
awk '75 > $3 && $3 >= 60 {print $1, $2, "3"}' "students.txt" | sort -rnk3 >> bigMen.txt
column -t bigMen.txt
awk '$3 < 60' "students.txt" | sort -rnk3 | column -t > smallMen.txt
echo "-----------"
cat smallMen.txt 

# Фамилия Имя Баллы [от 1 до 100]
# В одном скрипте студенты с 60+ и заменить на оценки от 60 до 75 - 3, от 70 до 90 - 4
# В остальном другие