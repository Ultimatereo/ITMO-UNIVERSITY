#!/bin/bash 
echo $$ > .pid
wins=0
mode="start"
usr1() 
{ 
	mode="win"
	wins=$((wins+1))
	echo Current number of wins: $wins
} 
trap 'usr1' SIGUSR1
usr2()
{
	mode="lose"
	echo "Unfortunately you lost :("
	exit 1
}
trap 'usr2' SIGUSR2
term()
{
	mode="quit"
	echo "QUIT"
	exit 0
}
trap 'term' SIGTERM

while true; do
	a=1+1
done