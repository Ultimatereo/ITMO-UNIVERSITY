#!/bin/bash
while true; do 
		echo $$ > .pid
        read line 
        echo "$line" > pipe 
done