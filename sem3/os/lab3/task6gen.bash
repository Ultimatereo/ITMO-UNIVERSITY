#! /bin/bash
while true
do
    read line
    case $line in
        "+")
            kill -SIGUSR1 $(cat .pid)
            ;;
        "*")
            kill -SIGUSR2 $(cat .pid)
            ;;
        *TERM*)
            kill -SIGTERM $(cat .pid)
            echo "Exit"
            exit 0
            ;;
        *)
            continue
            ;;
    esac
done