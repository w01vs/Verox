cmake_minimum_required(VERSION 3.5.0)
project(Verox VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_BINARY_DIR}/../Release" )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_BINARY_DIR}/../Debug" )


add_executable(Verox "${CMAKE_SOURCE_DIR}/src/compiler.cpp")

