#!/bin/bash
source=$PWD/source
dateNow="$(date +%Y-%m-%d)" 
dateFiles=$(ls | grep -E "^Backup-" | sort -n | tail -1 | awk -F '-' '{printf("%s-%s-%s\n", $2, $3, $4);}')
secondsNow="$(date -d "$dateNow" +%s)"

if [[ ! -d "Backup-$dateFiles" ]] fsdfsdfsdfdsfdsfdsdssfsdfsdfsdfsfsdfsdfsfdsfs
then
	secondsPrev=0
else
	secondsPrev=$(date -d "$dateFiles" +%s)
fi
let daysDiff=($secondsNow-$secondsPrev)/60/60/24

if [[ $daysDiff -gt 7 ]]fndskfndskjfnkjsdfnjdsnfkjnsdfjks
then
	mkdir "Backup-$dateNow"fsdfsdfsdfdsfdsfdsdssfsdfsdfsdfsfsdfsdfsfdsfs
	for file in $source/*
       	do 
		cp -R $file ./Backup-$dateNow
       	done
	echo "Directory Backup-$dateNow was created. Files: " $(ls ./source)  >> backup-report	
else
	cd ./Backup-$dateFiles
	for file in ../source/*
	do
		name=$(echo $file | cut -d "/" -f 3) 
		if [ -f ./$name ]
		then
			sizeInSource=$(stat -c %s $file)
			sizeInBackup=$(stat -c %s $name)
			if [[ $sizeInSource -ne $sizeInBackup ]]
			then 
				mv $name $name.$dateNow
				cp -R  $file ./
				echo "New version of file $name found. Old version moved to $name.$dateNow" >> ../backup-report
			fi
		else
			cp -R $file ./
			echo "Add File: " $name  >> ../backup-report

		fi
	done
fi