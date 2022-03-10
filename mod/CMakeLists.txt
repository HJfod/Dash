cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

set(CMAKE_BUILD_TYPE Release)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

project(GDML VERSION 0.1.0 LANGUAGES C CXX)

set(GEODE_NO_GEODE_FILE On)

file(GLOB_RECURSE SOURCES src/*.cpp)

add_definitions(/bigobj)
add_definitions(-DGDML_EXPORT)

add_library(GDML STATIC ${SOURCES})

include(../sdk/Geode.cmake)

add_subdirectory(../sdk/include/fmt ${CMAKE_BINARY_DIR}/fmt)

target_link_libraries(GDML fmt)
target_include_directories(GDML PUBLIC ../include)
