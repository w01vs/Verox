#!/bin/bash

# Function to build a specific configuration
build_config() {
    # Set the CC and CXX environment variables to use gcc and g++
    export CC=gcc
    export CXX=g++

    # Create and navigate to the build directory
    mkdir -p build
    cd build

    # Run CMake with the specified configuration and clean cache
    cmake -G "Ninja" -DCMAKE_BUILD_TYPE=$1 -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..

    # Build the project
    cmake --build .

    # Navigate back to the root directory
    cd ..
}

# Clean the build directory
rm -rf build

# Build the Debug configuration
build_config Debug

# Build the Release configuration
build_config Release