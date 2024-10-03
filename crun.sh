#!/bin/bash
./Release/comp temp.vx

if [ $? == 0 ]
then
    ./verox
    status=$?
    echo Finished
    echo exit code: $status
fi

if [ $? != 0 ]
then
    echo "fuck it didn't compile"
fi