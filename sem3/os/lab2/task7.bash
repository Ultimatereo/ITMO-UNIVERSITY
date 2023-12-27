#!/bin/bash
declare -A start
declare -A cmd
declare -A data
while [[ "$SECONDS" -le "60" ]]; do
    for pid in $(ps uax | awk 'FNR > 1 { print $2 }'); do
		if [[ "$pid" -ne "$$" ]]; then  
	        if [[ "${start[$pid]}" -eq "" ]]; then
	            if [ -r /proc/$pid/io ] && [ -r /proc/$pid/cmdline ]; then
		           	start[$pid]="$(grep "rchar" /proc/$pid/io | awk '{ print $2 }')"
		    		delta[$pid]="0"
			    	cmd[$pid]="$(cat /proc/$pid/cmdline | tr -d '\0')"
		        fi
		    else
		    	if [ -r /proc/$pid/io ]; then
		        	let delta[$pid]="$(grep "rchar" /proc/$pid/io | awk '{ print $2 }')"-${start[$pid]}
		        fi
	        fi
	    fi
    done
    echo -ne "$SECONDS\r"
done
echo "$(
for pid in "${!delta[@]}"; do
    echo "$pid ${delta[$pid]} ${cmd[$pid]}"
done)" | sort -rnk2 | head -n3 | awk '{ print $1":"$2":"$3 }'