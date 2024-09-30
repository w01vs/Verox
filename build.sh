#!/bin/bash

# Function to build a specific configuration
build_config() {
    # Run CMake with the specified configuration
    cmake -DCMAKE_BUILD_TYPE=$1 .

    # Build the project
    make
}

# Build the Debug configuration
build_config Debug

# Build the Release configuration
build_config Release