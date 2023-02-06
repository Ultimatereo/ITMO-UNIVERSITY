#!/bin/bash
echo "Hi! You can choose one of those following commands."
echo "1 = nano"
echo "2 = vim"
echo "3 = links"
echo "4 = exit"
read a
while [[ "$a" != 4 ]]
do 
	case $a in
		1 )
		/usr/bin/nano
		;;
		2 )
		/usr/bin/vim
		;;
		3 )
		/usr/bin/links
		;;
	esac
	read a
done
echo "change da world"
echo "my final message. Goodb ye"
exit 0
