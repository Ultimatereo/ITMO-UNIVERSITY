#!/bin/bash
mode="start"
wins=0
echo "start" > pipe

(tail -f pipe) | 
while true; do 
	read line;
	case $line in
        "win")
            wins=$((wins+1))
            echo Current number of wins: $wins
            ;;
        "lose")
            echo "Unfortunately you lost :("
            kill $(cat .pid)
            exit 0 # We can also change exit codes to distinguish cases "lose" and "QUIT" but dunno if it's needed
            ;;
        "QUIT")
            echo "QUIT"
            kill $(cat .pid)
            exit 0
            ;;
        *) 
	        # echo "Incorrect input"	
            # kill $(cat .pid)
            # exit 1
            ;;
    esac
done