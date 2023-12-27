#!/bin/bash
mode="+"
value=1

(tail -f pipe) | 
while true; do 
	read line;
	echo "$line"
	case $line in
        "+")
            mode="+"
            ;;
        "*")
            mode="*"
            ;;
        "QUIT")
            echo "Exit"
            kill $(cat .pid)
            exit 0
            ;;
        [0-9]* | [+-][0-9]*)
            case $mode in
                "+")
                    echo "Sum of " $value "and " $line						
                    value=$((value + line))
                    echo "Result: " $value
                    ;;
                "*")
                    echo "Multiply of " $value "and " $line						
                    value=$((value * line))
                    echo "Result: " $value
                    ;;
            esac
            ;;
        *) 
	        echo "Incorrect input"	
            kill $(cat .pid)
            exit 1
            ;;
    esac
done