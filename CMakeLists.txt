cmake_minimum_required(VERSION 3.20)

project(compiler)

set(CMAKE_CXX_STANDARD 23)
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR} )

add_executable(comp "src/compiler.cpp")