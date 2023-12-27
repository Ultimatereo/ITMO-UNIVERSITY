#!/bin/bash
b=""
read a
b="$b$a"
while [[ "$a" != "q" ]]
do 
	read a
	b="$b$a"
	if [[ "$a" == "q" ]]
		then break
	fi
done
echo "$b"
