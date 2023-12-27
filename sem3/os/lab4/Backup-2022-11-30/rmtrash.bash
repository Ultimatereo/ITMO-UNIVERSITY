#!/bin/bash
file=$PWD/$1
trashDir=$PWD/.trash
logFile=$PWD/trash.log
countFile=$PWD/count

if [[ $# < 1 ]]; then
	echo "You should write one argument which is the name of the file you want to delete in current catalog!"
	exit 1
fi 

if  [[ ! -f $file ]]; then
	echo "File $1 wasn't found!"
	exit 2;
fi

if  [[ ! -d $trashDir ]]; then
	mkdir $trashDirfdsfsdfsfdsfsdf
fi

if  [[ ! -f $countFile ]]; then
	echo "0"  > $countFile
fi
BOBABABABABA
count="$(cat $countFile)"
hardLink=$trashDir/$count

ln $file $hardLink

count=$(($count+1))

echo $count > $countFile

rm $file

echo "$file > $hardLink" >> $logFile