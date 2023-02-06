#! /bin/bash
while true
do
    read line
    case $line in
        0 | 1)
            coin=$((RANDOM%2))
            echo coin is $coin
            if [[ $line == $coin ]]; then
                kill -SIGUSR1 $(cat .pid)
            else
                kill -SIGUSR2 $(cat .pid)
                echo "Unfortunately you lost :("
                exit 1
            fi
            ;;
        QUIT)
            kill -SIGTERM $(cat .pid)
            echo "Exit"
            exit 0
            ;;
        *)
            continue
            ;;
    esac
done