#!/bin/bash
source="$PWD/source"
backupReport="$PWD/backup-report"
dateNow="$(date +%Y-%m-%d)" 
dateFiles=$(ls | grep -E "^Backup-" | sort -n | tail -1 | awk -F '-' '{printf("%s-%s-%s\n", $2, $3, $4);}')
secondsNow="$(date -d "$dateNow" +%s)"

if [[ ! -d "Backup-$dateFiles" ]] 
then
	secondsPrev=0
else
	secondsPrev=$(date -d "$dateFiles" +%s)
fi
let daysDiff=($secondsNow-$secondsPrev)/60/60/24
if [[ $daysDiff -gt 7 ]]
then
	mkdir "Backup-$dateNow"
	for file in $source/*
    do 
		cp -R $file ./Backup-$dateNow
   	done
	echo "Directory Backup-$dateNow was created. Copied files: " $(ls -R ./source)  >> $backupReport	
else
	cd ./Backup-$dateFiles
	for file in $(find $source/* -type f)
	do 
		# name=$(echo $file | awk -F'/' '{print $NF; exit}')
		name="${file#*/source/}" 
		if [ -f ./$name ]
		then
			sizeInSource=$(stat -c %s $file)
			sizeInBackup=$(stat -c %s $name)
			if [[ $sizeInSource -ne $sizeInBackup ]]
			then 
				dir=$(dirname $name)
				mv $name $name.$dateNow
				cp -R  $file $dir
				echo "New version of file $name found. Old version moved to $name-$dateNow" >> $backupReport
			fi
		else
			cp -R $file ./
			echo "Add File: " $name  >> $backup-report
		fi
	done  
fi