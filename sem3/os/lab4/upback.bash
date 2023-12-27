#!/bin/bash
dateFiles=$(ls | grep -E "^Backup-" | sort -n | tail -1 | awk -F '-' '{printf("%s-%s-%s\n", $2, $3, $4);}')
if [[ ! -d restore ]]
then
	mkdir restore
fi
restore="$PWD/restore"
cd ./Backup-$dateFiles

for file in $(find . -type f)
do
	filesNotToCopy="$(echo "$file" | grep -v -E "\.[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$")"
	if [ "$file" = "$filesNotToCopy" ] 
	then
		dir=$(dirname $file)
		dir=${dir:2}
		if [[ ! -d "$restore/$dir" ]]
		then
			mkdir -p "$restore/$dir"
		fi
		cp -R "$file" "$restore/$dir"
	fi
done

