#!/bin/bash
# ~/test <=> ./test
echo "start of report" > ./report
mkdir ./test >> /dev/null &&
{
 echo "catalog test was created successfully" >> ./report;
 touch ./test/"$(date +"%F_%T")" 
}

ping -q -c 1 -w 3 "net.nikogo.com" >> /dev/null || echo "ping failed" >> ./report
