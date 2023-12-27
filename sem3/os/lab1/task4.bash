#!/bin/bash
if [[ "$HOME" == "$PWD" ]]
	then echo "$HOME"
	exit 0
fi
echo "ERROR"
exit 1
