#!/bin/bash

./build.sh

if [ $? -eq 0 ]
then
    ./run.sh
fi