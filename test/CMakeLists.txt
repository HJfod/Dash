cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

project(HotUICpp VERSION 0.1.0 LANGUAGES C CXX)

set(GEODE_NO_GEODE_FILE Off)

file(GLOB_RECURSE SOURCES TestLayer.cpp testhook.cpp)
file(GLOB_RECURSE HEADERS ../include/*.hpp *.hpp)

set(SOURCE_FILES ${SOURCES})
include(../sdk/Geode.cmake)

target_precompile_headers(${PROJECT_NAME} PUBLIC ${HEADERS})

target_link_libraries(${PROJECT_NAME} GDML)
