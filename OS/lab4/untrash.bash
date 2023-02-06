#!/bin/bash
logFile=$PWD/trash.log

if [[ $# < 1 ]]; then
	echo "You should write one argument which is the name of the file you want to restore in current catalog!"
	exit 1
fi 
grep -P "(^[\w\/-]+\/$1)(?=\s)" $logFile | 
while read -r line ; do
	destFile=$(echo $line | awk '{print $1;}')
   	srcFile=$(echo $line | awk '{print $3;}')
	while true; do
		printf "Do you want to restore $destFile? (Y/N) "		
		read -r yn < /dev/tty
		if [[ $yn = "Y" || $yn = "N" ]]
		then
			break
		fi
	done
	if [ $yn = "Y" ]
	then 
		destFileDir=$(dirname $destFile)
		nameFile=$1
		if [[ ! -d $destFileDir ]]; then
        		echo "Original directory doesn't exist. Restoring into $PWD"
        		destFileDir = "$PWD"
        	fi
		mode=-1
		while [[ -f "$destFileDir/$nameFile" ]]; 
		do
			printf "File $nameFile exists in $destFileDir. What do you want to do with that? 0 - Rename it, 1 - Cancel restoring, 2 - Keep the name. "
			read -r mode < /dev/tty
			if [[ $mode = 0 || $mode = 1 || $mode = 2 ]]
			then
				break
			fi
		done

		if [ $mode = 0 ]
		then
			while [[ -f "$destFileDir/$nameFile" ]]
			do
				printf "File $nameFile exists in $destFileDir. Please enter a new name for restored file. "
				read -r nameFile < /dev/tty
			done
		elif [ $mode = 1 ]
		then
			continue
		elif [ $mode = 2 ] 
		then
			rm "$destFileDir/$nameFile"
		fi
		# Пользователь либо может переименовать файл, либо отменяем восстановление, либо восстанавливаем, удаляя то, что есть
		ln "$srcFile" "$destFileDir/$nameFile" && 
		rm "$srcFile" &&
       	{
			echo "$(grep -v "$srcFile" trash.log)" > trash.log	 
       		echo "$destFile is successfully restored in $destFileDir/$nameFile"
        }
	fi
done
