#!/bin/bash

./build.sh

if [ $? -eq 0 ]
then
    ./crun.sh
fi