cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

project(GDML VERSION 0.1.0 LANGUAGES C CXX)

file(GLOB_RECURSE SOURCES src/*.cpp)

add_definitions(/bigobj)
add_definitions(-DGDML_EXPORT)

set(SOURCE_FILES ${SOURCES})
include(../sdk/Geode.cmake)

add_subdirectory(../sdk/include/fmt ${CMAKE_BINARY_DIR}/fmt)

target_link_libraries(GDML fmt)
