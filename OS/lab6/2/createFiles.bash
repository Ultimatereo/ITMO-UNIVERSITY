#!/bin/bash
ind=1
end=$1
while true
do
	bash createFile.bash $ind 100000
	if [ $ind -gt $end ] 
	then
		break
	fi
	ind=$(($ind + 1))
done