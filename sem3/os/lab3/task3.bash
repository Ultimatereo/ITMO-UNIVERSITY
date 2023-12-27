#!/bin/bash
echo "5 * * * $(date +"%u" )  ~/LinuxCourse/lab3/task1.bash" | crontab