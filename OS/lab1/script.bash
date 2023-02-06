#!/bin/bash
let a=2*2-10
echo $a
let b=$1+$2
echo $1+$2=$b
echo DIRSTACK: $DIRSTACK
echo HOME: $HOME
echo HOSTNAME: $HOSTNAME
echo HOSTTYPE: $HOSTTYPE
echo PWD: $PWD
echo OSTYPE: $OSTYPE
echo PATH: $PATH
echo PPID: $PPID
echo number of args: $#
echo all args: $*
echo $@
echo PID of the last phone process: $!
echo PID of this script: $$
read v1 v2
let b=$v1+$v2
echo $b
echo SECONDS: $SECONDS