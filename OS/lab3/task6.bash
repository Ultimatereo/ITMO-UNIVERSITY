#!/bin/bash 

echo $$ > .pid

value=1
mode="+"
usr1() 
{ 
	mode="+"
} 
trap 'usr1' SIGUSR1
usr2()
{
	mode="*"
}
trap 'usr2' SIGUSR2
term()
{
	mode="exit"
}
trap 'term' SIGTERM


while true; do
    case $mode in
    	"+")
			let value=$value+2
			;;
		"*")
			let value=$value*2
			;;
		"exit")
     		echo "Exit"
     		exit 0
     		;;
    esac	
    echo $value
    sleep 1	
done