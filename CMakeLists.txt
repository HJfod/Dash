cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

project(GDML VERSION 0.1.0 LANGUAGES C CXX)

file(GLOB_RECURSE SOURCES src/*.cpp)

set(SOURCE_FILES ${SOURCES})
include(sdk/Geode.cmake)
