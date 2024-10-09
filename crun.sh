#!/bin/bash
./Release/comp temp.vx

if [ $? == 0 ]
then
    ./verox
    status=$?
    echo exit code: $status
    echo expected exit code: $1
    if [ $1 == $status ]
    then
        echo SUCCESS!!!!!
    else
        echo "OOPS SOMETHING WENT WRONG"
    fi
fi

if [ $? != 0 ]
then
    echo "fuck it didn't compile"
fi