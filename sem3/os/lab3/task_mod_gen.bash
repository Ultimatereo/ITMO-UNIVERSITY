#!/bin/bash
while true; do 
	echo $$ > .pid
    read line 
    coin=$((RANDOM%2))
    # coin=0
    case $line in
    	0 | 1)
			if [[ $line == $coin ]]; then
				echo "win" > pipe
			else
				echo "lose" > pipe
			fi
			;;
		*)
			echo "$line" > pipe 
			;;
	esac
done